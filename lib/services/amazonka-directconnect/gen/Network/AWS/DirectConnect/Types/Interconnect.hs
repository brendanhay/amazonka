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
import qualified Network.AWS.Prelude as Prelude

-- | Information about an interconnect.
--
-- /See:/ 'newInterconnect' smart constructor.
data Interconnect = Interconnect'
  { -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the interconnect.
    interconnectId :: Prelude.Maybe Prelude.Text,
    -- | The location of the connection.
    location :: Prelude.Maybe Prelude.Text,
    -- | The name of the interconnect.
    interconnectName :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDevice :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the interconnect supports a secondary BGP in the same
    -- address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Prelude.Maybe HasLogicalRedundancy,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent call to DescribeLoa for this connection.
    loaIssueTime :: Prelude.Maybe Core.POSIX,
    -- | The bandwidth of the connection.
    bandwidth :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
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
    interconnectState :: Prelude.Maybe InterconnectState,
    -- | The Amazon Web Services Region where the connection is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider associated with the interconnect.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the physical connection.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the interconnect.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Interconnect' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagId', 'interconnect_lagId' - The ID of the LAG.
--
-- 'interconnectId', 'interconnect_interconnectId' - The ID of the interconnect.
--
-- 'location', 'interconnect_location' - The location of the connection.
--
-- 'interconnectName', 'interconnect_interconnectName' - The name of the interconnect.
--
-- 'awsDevice', 'interconnect_awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'hasLogicalRedundancy', 'interconnect_hasLogicalRedundancy' - Indicates whether the interconnect supports a secondary BGP in the same
-- address family (IPv4\/IPv6).
--
-- 'awsLogicalDeviceId', 'interconnect_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'loaIssueTime', 'interconnect_loaIssueTime' - The time of the most recent call to DescribeLoa for this connection.
--
-- 'bandwidth', 'interconnect_bandwidth' - The bandwidth of the connection.
--
-- 'jumboFrameCapable', 'interconnect_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
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
-- 'region', 'interconnect_region' - The Amazon Web Services Region where the connection is located.
--
-- 'providerName', 'interconnect_providerName' - The name of the service provider associated with the interconnect.
--
-- 'awsDeviceV2', 'interconnect_awsDeviceV2' - The Direct Connect endpoint that terminates the physical connection.
--
-- 'tags', 'interconnect_tags' - The tags associated with the interconnect.
newInterconnect ::
  Interconnect
newInterconnect =
  Interconnect'
    { lagId = Prelude.Nothing,
      interconnectId = Prelude.Nothing,
      location = Prelude.Nothing,
      interconnectName = Prelude.Nothing,
      awsDevice = Prelude.Nothing,
      hasLogicalRedundancy = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      loaIssueTime = Prelude.Nothing,
      bandwidth = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      interconnectState = Prelude.Nothing,
      region = Prelude.Nothing,
      providerName = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the LAG.
interconnect_lagId :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_lagId = Lens.lens (\Interconnect' {lagId} -> lagId) (\s@Interconnect' {} a -> s {lagId = a} :: Interconnect)

-- | The ID of the interconnect.
interconnect_interconnectId :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_interconnectId = Lens.lens (\Interconnect' {interconnectId} -> interconnectId) (\s@Interconnect' {} a -> s {interconnectId = a} :: Interconnect)

-- | The location of the connection.
interconnect_location :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_location = Lens.lens (\Interconnect' {location} -> location) (\s@Interconnect' {} a -> s {location = a} :: Interconnect)

-- | The name of the interconnect.
interconnect_interconnectName :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_interconnectName = Lens.lens (\Interconnect' {interconnectName} -> interconnectName) (\s@Interconnect' {} a -> s {interconnectName = a} :: Interconnect)

-- | The Direct Connect endpoint on which the physical connection terminates.
interconnect_awsDevice :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_awsDevice = Lens.lens (\Interconnect' {awsDevice} -> awsDevice) (\s@Interconnect' {} a -> s {awsDevice = a} :: Interconnect)

-- | Indicates whether the interconnect supports a secondary BGP in the same
-- address family (IPv4\/IPv6).
interconnect_hasLogicalRedundancy :: Lens.Lens' Interconnect (Prelude.Maybe HasLogicalRedundancy)
interconnect_hasLogicalRedundancy = Lens.lens (\Interconnect' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Interconnect' {} a -> s {hasLogicalRedundancy = a} :: Interconnect)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
interconnect_awsLogicalDeviceId :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_awsLogicalDeviceId = Lens.lens (\Interconnect' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@Interconnect' {} a -> s {awsLogicalDeviceId = a} :: Interconnect)

-- | The time of the most recent call to DescribeLoa for this connection.
interconnect_loaIssueTime :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.UTCTime)
interconnect_loaIssueTime = Lens.lens (\Interconnect' {loaIssueTime} -> loaIssueTime) (\s@Interconnect' {} a -> s {loaIssueTime = a} :: Interconnect) Prelude.. Lens.mapping Core._Time

-- | The bandwidth of the connection.
interconnect_bandwidth :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_bandwidth = Lens.lens (\Interconnect' {bandwidth} -> bandwidth) (\s@Interconnect' {} a -> s {bandwidth = a} :: Interconnect)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
interconnect_jumboFrameCapable :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Bool)
interconnect_jumboFrameCapable = Lens.lens (\Interconnect' {jumboFrameCapable} -> jumboFrameCapable) (\s@Interconnect' {} a -> s {jumboFrameCapable = a} :: Interconnect)

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
interconnect_interconnectState :: Lens.Lens' Interconnect (Prelude.Maybe InterconnectState)
interconnect_interconnectState = Lens.lens (\Interconnect' {interconnectState} -> interconnectState) (\s@Interconnect' {} a -> s {interconnectState = a} :: Interconnect)

-- | The Amazon Web Services Region where the connection is located.
interconnect_region :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_region = Lens.lens (\Interconnect' {region} -> region) (\s@Interconnect' {} a -> s {region = a} :: Interconnect)

-- | The name of the service provider associated with the interconnect.
interconnect_providerName :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_providerName = Lens.lens (\Interconnect' {providerName} -> providerName) (\s@Interconnect' {} a -> s {providerName = a} :: Interconnect)

-- | The Direct Connect endpoint that terminates the physical connection.
interconnect_awsDeviceV2 :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_awsDeviceV2 = Lens.lens (\Interconnect' {awsDeviceV2} -> awsDeviceV2) (\s@Interconnect' {} a -> s {awsDeviceV2 = a} :: Interconnect)

-- | The tags associated with the interconnect.
interconnect_tags :: Lens.Lens' Interconnect (Prelude.Maybe (Prelude.NonEmpty Tag))
interconnect_tags = Lens.lens (\Interconnect' {tags} -> tags) (\s@Interconnect' {} a -> s {tags = a} :: Interconnect) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Interconnect where
  parseJSON =
    Core.withObject
      "Interconnect"
      ( \x ->
          Interconnect'
            Prelude.<$> (x Core..:? "lagId")
            Prelude.<*> (x Core..:? "interconnectId")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "interconnectName")
            Prelude.<*> (x Core..:? "awsDevice")
            Prelude.<*> (x Core..:? "hasLogicalRedundancy")
            Prelude.<*> (x Core..:? "awsLogicalDeviceId")
            Prelude.<*> (x Core..:? "loaIssueTime")
            Prelude.<*> (x Core..:? "bandwidth")
            Prelude.<*> (x Core..:? "jumboFrameCapable")
            Prelude.<*> (x Core..:? "interconnectState")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "providerName")
            Prelude.<*> (x Core..:? "awsDeviceV2")
            Prelude.<*> (x Core..:? "tags")
      )

instance Prelude.Hashable Interconnect

instance Prelude.NFData Interconnect
