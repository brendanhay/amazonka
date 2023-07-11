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
-- Module      : Amazonka.DirectConnect.Types.Interconnect
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.Interconnect where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types.HasLogicalRedundancy
import Amazonka.DirectConnect.Types.InterconnectState
import Amazonka.DirectConnect.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Information about an interconnect.
--
-- /See:/ 'newInterconnect' smart constructor.
data Interconnect = Interconnect'
  { -- | The Direct Connect endpoint on which the physical connection terminates.
    awsDevice :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the physical connection.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The bandwidth of the connection.
    bandwidth :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the interconnect supports a secondary BGP in the same
    -- address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Prelude.Maybe HasLogicalRedundancy,
    -- | The ID of the interconnect.
    interconnectId :: Prelude.Maybe Prelude.Text,
    -- | The name of the interconnect.
    interconnectName :: Prelude.Maybe Prelude.Text,
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
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | The time of the most recent call to DescribeLoa for this connection.
    loaIssueTime :: Prelude.Maybe Data.POSIX,
    -- | The location of the connection.
    location :: Prelude.Maybe Prelude.Text,
    -- | The name of the service provider associated with the interconnect.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the connection is located.
    region :: Prelude.Maybe Prelude.Text,
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
-- 'awsDevice', 'interconnect_awsDevice' - The Direct Connect endpoint on which the physical connection terminates.
--
-- 'awsDeviceV2', 'interconnect_awsDeviceV2' - The Direct Connect endpoint that terminates the physical connection.
--
-- 'awsLogicalDeviceId', 'interconnect_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'bandwidth', 'interconnect_bandwidth' - The bandwidth of the connection.
--
-- 'hasLogicalRedundancy', 'interconnect_hasLogicalRedundancy' - Indicates whether the interconnect supports a secondary BGP in the same
-- address family (IPv4\/IPv6).
--
-- 'interconnectId', 'interconnect_interconnectId' - The ID of the interconnect.
--
-- 'interconnectName', 'interconnect_interconnectName' - The name of the interconnect.
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
-- 'jumboFrameCapable', 'interconnect_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'lagId', 'interconnect_lagId' - The ID of the LAG.
--
-- 'loaIssueTime', 'interconnect_loaIssueTime' - The time of the most recent call to DescribeLoa for this connection.
--
-- 'location', 'interconnect_location' - The location of the connection.
--
-- 'providerName', 'interconnect_providerName' - The name of the service provider associated with the interconnect.
--
-- 'region', 'interconnect_region' - The Amazon Web Services Region where the connection is located.
--
-- 'tags', 'interconnect_tags' - The tags associated with the interconnect.
newInterconnect ::
  Interconnect
newInterconnect =
  Interconnect'
    { awsDevice = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      bandwidth = Prelude.Nothing,
      hasLogicalRedundancy = Prelude.Nothing,
      interconnectId = Prelude.Nothing,
      interconnectName = Prelude.Nothing,
      interconnectState = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      lagId = Prelude.Nothing,
      loaIssueTime = Prelude.Nothing,
      location = Prelude.Nothing,
      providerName = Prelude.Nothing,
      region = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Direct Connect endpoint on which the physical connection terminates.
interconnect_awsDevice :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_awsDevice = Lens.lens (\Interconnect' {awsDevice} -> awsDevice) (\s@Interconnect' {} a -> s {awsDevice = a} :: Interconnect)

-- | The Direct Connect endpoint that terminates the physical connection.
interconnect_awsDeviceV2 :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_awsDeviceV2 = Lens.lens (\Interconnect' {awsDeviceV2} -> awsDeviceV2) (\s@Interconnect' {} a -> s {awsDeviceV2 = a} :: Interconnect)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
interconnect_awsLogicalDeviceId :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_awsLogicalDeviceId = Lens.lens (\Interconnect' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@Interconnect' {} a -> s {awsLogicalDeviceId = a} :: Interconnect)

-- | The bandwidth of the connection.
interconnect_bandwidth :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_bandwidth = Lens.lens (\Interconnect' {bandwidth} -> bandwidth) (\s@Interconnect' {} a -> s {bandwidth = a} :: Interconnect)

-- | Indicates whether the interconnect supports a secondary BGP in the same
-- address family (IPv4\/IPv6).
interconnect_hasLogicalRedundancy :: Lens.Lens' Interconnect (Prelude.Maybe HasLogicalRedundancy)
interconnect_hasLogicalRedundancy = Lens.lens (\Interconnect' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Interconnect' {} a -> s {hasLogicalRedundancy = a} :: Interconnect)

-- | The ID of the interconnect.
interconnect_interconnectId :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_interconnectId = Lens.lens (\Interconnect' {interconnectId} -> interconnectId) (\s@Interconnect' {} a -> s {interconnectId = a} :: Interconnect)

-- | The name of the interconnect.
interconnect_interconnectName :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_interconnectName = Lens.lens (\Interconnect' {interconnectName} -> interconnectName) (\s@Interconnect' {} a -> s {interconnectName = a} :: Interconnect)

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

-- | Indicates whether jumbo frames (9001 MTU) are supported.
interconnect_jumboFrameCapable :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Bool)
interconnect_jumboFrameCapable = Lens.lens (\Interconnect' {jumboFrameCapable} -> jumboFrameCapable) (\s@Interconnect' {} a -> s {jumboFrameCapable = a} :: Interconnect)

-- | The ID of the LAG.
interconnect_lagId :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_lagId = Lens.lens (\Interconnect' {lagId} -> lagId) (\s@Interconnect' {} a -> s {lagId = a} :: Interconnect)

-- | The time of the most recent call to DescribeLoa for this connection.
interconnect_loaIssueTime :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.UTCTime)
interconnect_loaIssueTime = Lens.lens (\Interconnect' {loaIssueTime} -> loaIssueTime) (\s@Interconnect' {} a -> s {loaIssueTime = a} :: Interconnect) Prelude.. Lens.mapping Data._Time

-- | The location of the connection.
interconnect_location :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_location = Lens.lens (\Interconnect' {location} -> location) (\s@Interconnect' {} a -> s {location = a} :: Interconnect)

-- | The name of the service provider associated with the interconnect.
interconnect_providerName :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_providerName = Lens.lens (\Interconnect' {providerName} -> providerName) (\s@Interconnect' {} a -> s {providerName = a} :: Interconnect)

-- | The Amazon Web Services Region where the connection is located.
interconnect_region :: Lens.Lens' Interconnect (Prelude.Maybe Prelude.Text)
interconnect_region = Lens.lens (\Interconnect' {region} -> region) (\s@Interconnect' {} a -> s {region = a} :: Interconnect)

-- | The tags associated with the interconnect.
interconnect_tags :: Lens.Lens' Interconnect (Prelude.Maybe (Prelude.NonEmpty Tag))
interconnect_tags = Lens.lens (\Interconnect' {tags} -> tags) (\s@Interconnect' {} a -> s {tags = a} :: Interconnect) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Interconnect where
  parseJSON =
    Data.withObject
      "Interconnect"
      ( \x ->
          Interconnect'
            Prelude.<$> (x Data..:? "awsDevice")
            Prelude.<*> (x Data..:? "awsDeviceV2")
            Prelude.<*> (x Data..:? "awsLogicalDeviceId")
            Prelude.<*> (x Data..:? "bandwidth")
            Prelude.<*> (x Data..:? "hasLogicalRedundancy")
            Prelude.<*> (x Data..:? "interconnectId")
            Prelude.<*> (x Data..:? "interconnectName")
            Prelude.<*> (x Data..:? "interconnectState")
            Prelude.<*> (x Data..:? "jumboFrameCapable")
            Prelude.<*> (x Data..:? "lagId")
            Prelude.<*> (x Data..:? "loaIssueTime")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "providerName")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "tags")
      )

instance Prelude.Hashable Interconnect where
  hashWithSalt _salt Interconnect' {..} =
    _salt
      `Prelude.hashWithSalt` awsDevice
      `Prelude.hashWithSalt` awsDeviceV2
      `Prelude.hashWithSalt` awsLogicalDeviceId
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` hasLogicalRedundancy
      `Prelude.hashWithSalt` interconnectId
      `Prelude.hashWithSalt` interconnectName
      `Prelude.hashWithSalt` interconnectState
      `Prelude.hashWithSalt` jumboFrameCapable
      `Prelude.hashWithSalt` lagId
      `Prelude.hashWithSalt` loaIssueTime
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Interconnect where
  rnf Interconnect' {..} =
    Prelude.rnf awsDevice
      `Prelude.seq` Prelude.rnf awsDeviceV2
      `Prelude.seq` Prelude.rnf awsLogicalDeviceId
      `Prelude.seq` Prelude.rnf bandwidth
      `Prelude.seq` Prelude.rnf hasLogicalRedundancy
      `Prelude.seq` Prelude.rnf interconnectId
      `Prelude.seq` Prelude.rnf interconnectName
      `Prelude.seq` Prelude.rnf interconnectState
      `Prelude.seq` Prelude.rnf jumboFrameCapable
      `Prelude.seq` Prelude.rnf lagId
      `Prelude.seq` Prelude.rnf loaIssueTime
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf tags
