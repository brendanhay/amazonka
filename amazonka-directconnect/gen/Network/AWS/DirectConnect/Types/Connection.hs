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
-- Module      : Network.AWS.DirectConnect.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Connection where

import Network.AWS.DirectConnect.Types.ConnectionState
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about an AWS Direct Connect connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The bandwidth of the connection.
    bandwidth :: Prelude.Maybe Prelude.Text,
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
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider associated with the connection.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the connection supports a secondary BGP peer in the
    -- same address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Prelude.Maybe HasLogicalRedundancy,
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDevice :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The name of the AWS Direct Connect service provider associated with the
    -- connection.
    partnerName :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the connection.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The time of the most recent call to DescribeLoa for this connection.
    loaIssueTime :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the AWS account that owns the connection.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region where the connection is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The location of the connection.
    location :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidth', 'connection_bandwidth' - The bandwidth of the connection.
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
-- 'awsDeviceV2', 'connection_awsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'connectionName', 'connection_connectionName' - The name of the connection.
--
-- 'providerName', 'connection_providerName' - The name of the service provider associated with the connection.
--
-- 'connectionId', 'connection_connectionId' - The ID of the connection.
--
-- 'hasLogicalRedundancy', 'connection_hasLogicalRedundancy' - Indicates whether the connection supports a secondary BGP peer in the
-- same address family (IPv4\/IPv6).
--
-- 'awsDevice', 'connection_awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'jumboFrameCapable', 'connection_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'lagId', 'connection_lagId' - The ID of the LAG.
--
-- 'partnerName', 'connection_partnerName' - The name of the AWS Direct Connect service provider associated with the
-- connection.
--
-- 'tags', 'connection_tags' - The tags associated with the connection.
--
-- 'loaIssueTime', 'connection_loaIssueTime' - The time of the most recent call to DescribeLoa for this connection.
--
-- 'ownerAccount', 'connection_ownerAccount' - The ID of the AWS account that owns the connection.
--
-- 'region', 'connection_region' - The AWS Region where the connection is located.
--
-- 'location', 'connection_location' - The location of the connection.
--
-- 'vlan', 'connection_vlan' - The ID of the VLAN.
newConnection ::
  Connection
newConnection =
  Connection'
    { bandwidth = Prelude.Nothing,
      connectionState = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      providerName = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      hasLogicalRedundancy = Prelude.Nothing,
      awsDevice = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      lagId = Prelude.Nothing,
      partnerName = Prelude.Nothing,
      tags = Prelude.Nothing,
      loaIssueTime = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      region = Prelude.Nothing,
      location = Prelude.Nothing,
      vlan = Prelude.Nothing
    }

-- | The bandwidth of the connection.
connection_bandwidth :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_bandwidth = Lens.lens (\Connection' {bandwidth} -> bandwidth) (\s@Connection' {} a -> s {bandwidth = a} :: Connection)

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

-- | The Direct Connect endpoint on which the physical connection terminates.
connection_awsDeviceV2 :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_awsDeviceV2 = Lens.lens (\Connection' {awsDeviceV2} -> awsDeviceV2) (\s@Connection' {} a -> s {awsDeviceV2 = a} :: Connection)

-- | The name of the connection.
connection_connectionName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionName = Lens.lens (\Connection' {connectionName} -> connectionName) (\s@Connection' {} a -> s {connectionName = a} :: Connection)

-- | The name of the service provider associated with the connection.
connection_providerName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_providerName = Lens.lens (\Connection' {providerName} -> providerName) (\s@Connection' {} a -> s {providerName = a} :: Connection)

-- | The ID of the connection.
connection_connectionId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionId = Lens.lens (\Connection' {connectionId} -> connectionId) (\s@Connection' {} a -> s {connectionId = a} :: Connection)

-- | Indicates whether the connection supports a secondary BGP peer in the
-- same address family (IPv4\/IPv6).
connection_hasLogicalRedundancy :: Lens.Lens' Connection (Prelude.Maybe HasLogicalRedundancy)
connection_hasLogicalRedundancy = Lens.lens (\Connection' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Connection' {} a -> s {hasLogicalRedundancy = a} :: Connection)

-- | The Direct Connect endpoint on which the physical connection terminates.
connection_awsDevice :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_awsDevice = Lens.lens (\Connection' {awsDevice} -> awsDevice) (\s@Connection' {} a -> s {awsDevice = a} :: Connection)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
connection_jumboFrameCapable :: Lens.Lens' Connection (Prelude.Maybe Prelude.Bool)
connection_jumboFrameCapable = Lens.lens (\Connection' {jumboFrameCapable} -> jumboFrameCapable) (\s@Connection' {} a -> s {jumboFrameCapable = a} :: Connection)

-- | The ID of the LAG.
connection_lagId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_lagId = Lens.lens (\Connection' {lagId} -> lagId) (\s@Connection' {} a -> s {lagId = a} :: Connection)

-- | The name of the AWS Direct Connect service provider associated with the
-- connection.
connection_partnerName :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_partnerName = Lens.lens (\Connection' {partnerName} -> partnerName) (\s@Connection' {} a -> s {partnerName = a} :: Connection)

-- | The tags associated with the connection.
connection_tags :: Lens.Lens' Connection (Prelude.Maybe (Prelude.NonEmpty Tag))
connection_tags = Lens.lens (\Connection' {tags} -> tags) (\s@Connection' {} a -> s {tags = a} :: Connection) Prelude.. Lens.mapping Prelude._Coerce

-- | The time of the most recent call to DescribeLoa for this connection.
connection_loaIssueTime :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_loaIssueTime = Lens.lens (\Connection' {loaIssueTime} -> loaIssueTime) (\s@Connection' {} a -> s {loaIssueTime = a} :: Connection) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the AWS account that owns the connection.
connection_ownerAccount :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_ownerAccount = Lens.lens (\Connection' {ownerAccount} -> ownerAccount) (\s@Connection' {} a -> s {ownerAccount = a} :: Connection)

-- | The AWS Region where the connection is located.
connection_region :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_region = Lens.lens (\Connection' {region} -> region) (\s@Connection' {} a -> s {region = a} :: Connection)

-- | The location of the connection.
connection_location :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_location = Lens.lens (\Connection' {location} -> location) (\s@Connection' {} a -> s {location = a} :: Connection)

-- | The ID of the VLAN.
connection_vlan :: Lens.Lens' Connection (Prelude.Maybe Prelude.Int)
connection_vlan = Lens.lens (\Connection' {vlan} -> vlan) (\s@Connection' {} a -> s {vlan = a} :: Connection)

instance Prelude.FromJSON Connection where
  parseJSON =
    Prelude.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Prelude..:? "bandwidth")
            Prelude.<*> (x Prelude..:? "connectionState")
            Prelude.<*> (x Prelude..:? "awsDeviceV2")
            Prelude.<*> (x Prelude..:? "connectionName")
            Prelude.<*> (x Prelude..:? "providerName")
            Prelude.<*> (x Prelude..:? "connectionId")
            Prelude.<*> (x Prelude..:? "hasLogicalRedundancy")
            Prelude.<*> (x Prelude..:? "awsDevice")
            Prelude.<*> (x Prelude..:? "jumboFrameCapable")
            Prelude.<*> (x Prelude..:? "lagId")
            Prelude.<*> (x Prelude..:? "partnerName")
            Prelude.<*> (x Prelude..:? "tags")
            Prelude.<*> (x Prelude..:? "loaIssueTime")
            Prelude.<*> (x Prelude..:? "ownerAccount")
            Prelude.<*> (x Prelude..:? "region")
            Prelude.<*> (x Prelude..:? "location")
            Prelude.<*> (x Prelude..:? "vlan")
      )

instance Prelude.Hashable Connection

instance Prelude.NFData Connection
