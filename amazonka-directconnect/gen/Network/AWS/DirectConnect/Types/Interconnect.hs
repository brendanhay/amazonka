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
-- Module      : Network.AWS.DirectConnect.Types.Interconnect
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.Interconnect where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types.HasLogicalRedundancy
import Network.AWS.DirectConnect.Types.InterconnectState
import Network.AWS.DirectConnect.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Information about an interconnect.
--
-- /See:/ 'newInterconnect' smart constructor.
data Interconnect = Interconnect'
  { -- | The bandwidth of the connection.
    bandwidth :: Core.Maybe Core.Text,
    -- | The ID of the interconnect.
    interconnectId :: Core.Maybe Core.Text,
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDeviceV2 :: Core.Maybe Core.Text,
    -- | The name of the service provider associated with the interconnect.
    providerName :: Core.Maybe Core.Text,
    -- | Indicates whether the interconnect supports a secondary BGP in the same
    -- address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Core.Maybe HasLogicalRedundancy,
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDevice :: Core.Maybe Core.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Core.Maybe Core.Bool,
    -- | The ID of the LAG.
    lagId :: Core.Maybe Core.Text,
    -- | The tags associated with the interconnect.
    tags :: Core.Maybe (Core.NonEmpty Tag),
    -- | The time of the most recent call to DescribeLoa for this connection.
    loaIssueTime :: Core.Maybe Core.POSIX,
    -- | The AWS Region where the connection is located.
    region :: Core.Maybe Core.Text,
    -- | The state of the interconnect. The following are the possible values:
    --
    -- -   @requested@: The initial state of an interconnect. The interconnect
    --     stays in the requested state until the Letter of Authorization (LOA)
    --     is sent to the customer.
    --
    -- -   @pending@: The interconnect is approved, and is being initialized.
    --
    -- -   @available@: The network link is up, and the interconnect is ready
    --     for use.
    --
    -- -   @down@: The network link is down.
    --
    -- -   @deleting@: The interconnect is being deleted.
    --
    -- -   @deleted@: The interconnect is deleted.
    --
    -- -   @unknown@: The state of the interconnect is not available.
    interconnectState :: Core.Maybe InterconnectState,
    -- | The location of the connection.
    location :: Core.Maybe Core.Text,
    -- | The name of the interconnect.
    interconnectName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Interconnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidth', 'interconnect_bandwidth' - The bandwidth of the connection.
--
-- 'interconnectId', 'interconnect_interconnectId' - The ID of the interconnect.
--
-- 'awsDeviceV2', 'interconnect_awsDeviceV2' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'providerName', 'interconnect_providerName' - The name of the service provider associated with the interconnect.
--
-- 'hasLogicalRedundancy', 'interconnect_hasLogicalRedundancy' - Indicates whether the interconnect supports a secondary BGP in the same
-- address family (IPv4\/IPv6).
--
-- 'awsDevice', 'interconnect_awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'jumboFrameCapable', 'interconnect_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'lagId', 'interconnect_lagId' - The ID of the LAG.
--
-- 'tags', 'interconnect_tags' - The tags associated with the interconnect.
--
-- 'loaIssueTime', 'interconnect_loaIssueTime' - The time of the most recent call to DescribeLoa for this connection.
--
-- 'region', 'interconnect_region' - The AWS Region where the connection is located.
--
-- 'interconnectState', 'interconnect_interconnectState' - The state of the interconnect. The following are the possible values:
--
-- -   @requested@: The initial state of an interconnect. The interconnect
--     stays in the requested state until the Letter of Authorization (LOA)
--     is sent to the customer.
--
-- -   @pending@: The interconnect is approved, and is being initialized.
--
-- -   @available@: The network link is up, and the interconnect is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The interconnect is being deleted.
--
-- -   @deleted@: The interconnect is deleted.
--
-- -   @unknown@: The state of the interconnect is not available.
--
-- 'location', 'interconnect_location' - The location of the connection.
--
-- 'interconnectName', 'interconnect_interconnectName' - The name of the interconnect.
newInterconnect ::
  Interconnect
newInterconnect =
  Interconnect'
    { bandwidth = Core.Nothing,
      interconnectId = Core.Nothing,
      awsDeviceV2 = Core.Nothing,
      providerName = Core.Nothing,
      hasLogicalRedundancy = Core.Nothing,
      awsDevice = Core.Nothing,
      jumboFrameCapable = Core.Nothing,
      lagId = Core.Nothing,
      tags = Core.Nothing,
      loaIssueTime = Core.Nothing,
      region = Core.Nothing,
      interconnectState = Core.Nothing,
      location = Core.Nothing,
      interconnectName = Core.Nothing
    }

-- | The bandwidth of the connection.
interconnect_bandwidth :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_bandwidth = Lens.lens (\Interconnect' {bandwidth} -> bandwidth) (\s@Interconnect' {} a -> s {bandwidth = a} :: Interconnect)

-- | The ID of the interconnect.
interconnect_interconnectId :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_interconnectId = Lens.lens (\Interconnect' {interconnectId} -> interconnectId) (\s@Interconnect' {} a -> s {interconnectId = a} :: Interconnect)

-- | The Direct Connect endpoint on which the physical connection terminates.
interconnect_awsDeviceV2 :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_awsDeviceV2 = Lens.lens (\Interconnect' {awsDeviceV2} -> awsDeviceV2) (\s@Interconnect' {} a -> s {awsDeviceV2 = a} :: Interconnect)

-- | The name of the service provider associated with the interconnect.
interconnect_providerName :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_providerName = Lens.lens (\Interconnect' {providerName} -> providerName) (\s@Interconnect' {} a -> s {providerName = a} :: Interconnect)

-- | Indicates whether the interconnect supports a secondary BGP in the same
-- address family (IPv4\/IPv6).
interconnect_hasLogicalRedundancy :: Lens.Lens' Interconnect (Core.Maybe HasLogicalRedundancy)
interconnect_hasLogicalRedundancy = Lens.lens (\Interconnect' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Interconnect' {} a -> s {hasLogicalRedundancy = a} :: Interconnect)

-- | The Direct Connect endpoint on which the physical connection terminates.
interconnect_awsDevice :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_awsDevice = Lens.lens (\Interconnect' {awsDevice} -> awsDevice) (\s@Interconnect' {} a -> s {awsDevice = a} :: Interconnect)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
interconnect_jumboFrameCapable :: Lens.Lens' Interconnect (Core.Maybe Core.Bool)
interconnect_jumboFrameCapable = Lens.lens (\Interconnect' {jumboFrameCapable} -> jumboFrameCapable) (\s@Interconnect' {} a -> s {jumboFrameCapable = a} :: Interconnect)

-- | The ID of the LAG.
interconnect_lagId :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_lagId = Lens.lens (\Interconnect' {lagId} -> lagId) (\s@Interconnect' {} a -> s {lagId = a} :: Interconnect)

-- | The tags associated with the interconnect.
interconnect_tags :: Lens.Lens' Interconnect (Core.Maybe (Core.NonEmpty Tag))
interconnect_tags = Lens.lens (\Interconnect' {tags} -> tags) (\s@Interconnect' {} a -> s {tags = a} :: Interconnect) Core.. Lens.mapping Lens._Coerce

-- | The time of the most recent call to DescribeLoa for this connection.
interconnect_loaIssueTime :: Lens.Lens' Interconnect (Core.Maybe Core.UTCTime)
interconnect_loaIssueTime = Lens.lens (\Interconnect' {loaIssueTime} -> loaIssueTime) (\s@Interconnect' {} a -> s {loaIssueTime = a} :: Interconnect) Core.. Lens.mapping Core._Time

-- | The AWS Region where the connection is located.
interconnect_region :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_region = Lens.lens (\Interconnect' {region} -> region) (\s@Interconnect' {} a -> s {region = a} :: Interconnect)

-- | The state of the interconnect. The following are the possible values:
--
-- -   @requested@: The initial state of an interconnect. The interconnect
--     stays in the requested state until the Letter of Authorization (LOA)
--     is sent to the customer.
--
-- -   @pending@: The interconnect is approved, and is being initialized.
--
-- -   @available@: The network link is up, and the interconnect is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The interconnect is being deleted.
--
-- -   @deleted@: The interconnect is deleted.
--
-- -   @unknown@: The state of the interconnect is not available.
interconnect_interconnectState :: Lens.Lens' Interconnect (Core.Maybe InterconnectState)
interconnect_interconnectState = Lens.lens (\Interconnect' {interconnectState} -> interconnectState) (\s@Interconnect' {} a -> s {interconnectState = a} :: Interconnect)

-- | The location of the connection.
interconnect_location :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_location = Lens.lens (\Interconnect' {location} -> location) (\s@Interconnect' {} a -> s {location = a} :: Interconnect)

-- | The name of the interconnect.
interconnect_interconnectName :: Lens.Lens' Interconnect (Core.Maybe Core.Text)
interconnect_interconnectName = Lens.lens (\Interconnect' {interconnectName} -> interconnectName) (\s@Interconnect' {} a -> s {interconnectName = a} :: Interconnect)

instance Core.FromJSON Interconnect where
  parseJSON =
    Core.withObject
      "Interconnect"
      ( \x ->
          Interconnect'
            Core.<$> (x Core..:? "bandwidth")
            Core.<*> (x Core..:? "interconnectId")
            Core.<*> (x Core..:? "awsDeviceV2")
            Core.<*> (x Core..:? "providerName")
            Core.<*> (x Core..:? "hasLogicalRedundancy")
            Core.<*> (x Core..:? "awsDevice")
            Core.<*> (x Core..:? "jumboFrameCapable")
            Core.<*> (x Core..:? "lagId")
            Core.<*> (x Core..:? "tags")
            Core.<*> (x Core..:? "loaIssueTime")
            Core.<*> (x Core..:? "region")
            Core.<*> (x Core..:? "interconnectState")
            Core.<*> (x Core..:? "location")
            Core.<*> (x Core..:? "interconnectName")
      )

instance Core.Hashable Interconnect

instance Core.NFData Interconnect
