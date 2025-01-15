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
-- Module      : Amazonka.NetworkFirewall.Types.FirewallStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.FirewallStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkFirewall.Types.CapacityUsageSummary
import Amazonka.NetworkFirewall.Types.ConfigurationSyncState
import Amazonka.NetworkFirewall.Types.FirewallStatusValue
import Amazonka.NetworkFirewall.Types.SyncState
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the current status of a Firewall. You can
-- retrieve this for a firewall by calling DescribeFirewall and providing
-- the firewall name and ARN.
--
-- /See:/ 'newFirewallStatus' smart constructor.
data FirewallStatus = FirewallStatus'
  { -- | Describes the capacity usage of the resources contained in a firewall\'s
    -- reference sets. Network Firewall calclulates the capacity usage by
    -- taking an aggregated count of all of the resources used by all of the
    -- reference sets in a firewall.
    capacityUsageSummary :: Prelude.Maybe CapacityUsageSummary,
    -- | The subnets that you\'ve configured for use by the Network Firewall
    -- firewall. This contains one array element per Availability Zone where
    -- you\'ve configured a subnet. These objects provide details of the
    -- information that is summarized in the @ConfigurationSyncStateSummary@
    -- and @Status@, broken down by zone and configuration object.
    syncStates :: Prelude.Maybe (Prelude.HashMap Prelude.Text SyncState),
    -- | The readiness of the configured firewall to handle network traffic
    -- across all of the Availability Zones where you\'ve configured it. This
    -- setting is @READY@ only when the @ConfigurationSyncStateSummary@ value
    -- is @IN_SYNC@ and the @Attachment@ @Status@ values for all of the
    -- configured subnets are @READY@.
    status :: FirewallStatusValue,
    -- | The configuration sync state for the firewall. This summarizes the sync
    -- states reported in the @Config@ settings for all of the Availability
    -- Zones where you have configured the firewall.
    --
    -- When you create a firewall or update its configuration, for example by
    -- adding a rule group to its firewall policy, Network Firewall distributes
    -- the configuration changes to all zones where the firewall is in use.
    -- This summary indicates whether the configuration changes have been
    -- applied everywhere.
    --
    -- This status must be @IN_SYNC@ for the firewall to be ready for use, but
    -- it doesn\'t indicate that the firewall is ready. The @Status@ setting
    -- indicates firewall readiness.
    configurationSyncStateSummary :: ConfigurationSyncState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirewallStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityUsageSummary', 'firewallStatus_capacityUsageSummary' - Describes the capacity usage of the resources contained in a firewall\'s
-- reference sets. Network Firewall calclulates the capacity usage by
-- taking an aggregated count of all of the resources used by all of the
-- reference sets in a firewall.
--
-- 'syncStates', 'firewallStatus_syncStates' - The subnets that you\'ve configured for use by the Network Firewall
-- firewall. This contains one array element per Availability Zone where
-- you\'ve configured a subnet. These objects provide details of the
-- information that is summarized in the @ConfigurationSyncStateSummary@
-- and @Status@, broken down by zone and configuration object.
--
-- 'status', 'firewallStatus_status' - The readiness of the configured firewall to handle network traffic
-- across all of the Availability Zones where you\'ve configured it. This
-- setting is @READY@ only when the @ConfigurationSyncStateSummary@ value
-- is @IN_SYNC@ and the @Attachment@ @Status@ values for all of the
-- configured subnets are @READY@.
--
-- 'configurationSyncStateSummary', 'firewallStatus_configurationSyncStateSummary' - The configuration sync state for the firewall. This summarizes the sync
-- states reported in the @Config@ settings for all of the Availability
-- Zones where you have configured the firewall.
--
-- When you create a firewall or update its configuration, for example by
-- adding a rule group to its firewall policy, Network Firewall distributes
-- the configuration changes to all zones where the firewall is in use.
-- This summary indicates whether the configuration changes have been
-- applied everywhere.
--
-- This status must be @IN_SYNC@ for the firewall to be ready for use, but
-- it doesn\'t indicate that the firewall is ready. The @Status@ setting
-- indicates firewall readiness.
newFirewallStatus ::
  -- | 'status'
  FirewallStatusValue ->
  -- | 'configurationSyncStateSummary'
  ConfigurationSyncState ->
  FirewallStatus
newFirewallStatus
  pStatus_
  pConfigurationSyncStateSummary_ =
    FirewallStatus'
      { capacityUsageSummary =
          Prelude.Nothing,
        syncStates = Prelude.Nothing,
        status = pStatus_,
        configurationSyncStateSummary =
          pConfigurationSyncStateSummary_
      }

-- | Describes the capacity usage of the resources contained in a firewall\'s
-- reference sets. Network Firewall calclulates the capacity usage by
-- taking an aggregated count of all of the resources used by all of the
-- reference sets in a firewall.
firewallStatus_capacityUsageSummary :: Lens.Lens' FirewallStatus (Prelude.Maybe CapacityUsageSummary)
firewallStatus_capacityUsageSummary = Lens.lens (\FirewallStatus' {capacityUsageSummary} -> capacityUsageSummary) (\s@FirewallStatus' {} a -> s {capacityUsageSummary = a} :: FirewallStatus)

-- | The subnets that you\'ve configured for use by the Network Firewall
-- firewall. This contains one array element per Availability Zone where
-- you\'ve configured a subnet. These objects provide details of the
-- information that is summarized in the @ConfigurationSyncStateSummary@
-- and @Status@, broken down by zone and configuration object.
firewallStatus_syncStates :: Lens.Lens' FirewallStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text SyncState))
firewallStatus_syncStates = Lens.lens (\FirewallStatus' {syncStates} -> syncStates) (\s@FirewallStatus' {} a -> s {syncStates = a} :: FirewallStatus) Prelude.. Lens.mapping Lens.coerced

-- | The readiness of the configured firewall to handle network traffic
-- across all of the Availability Zones where you\'ve configured it. This
-- setting is @READY@ only when the @ConfigurationSyncStateSummary@ value
-- is @IN_SYNC@ and the @Attachment@ @Status@ values for all of the
-- configured subnets are @READY@.
firewallStatus_status :: Lens.Lens' FirewallStatus FirewallStatusValue
firewallStatus_status = Lens.lens (\FirewallStatus' {status} -> status) (\s@FirewallStatus' {} a -> s {status = a} :: FirewallStatus)

-- | The configuration sync state for the firewall. This summarizes the sync
-- states reported in the @Config@ settings for all of the Availability
-- Zones where you have configured the firewall.
--
-- When you create a firewall or update its configuration, for example by
-- adding a rule group to its firewall policy, Network Firewall distributes
-- the configuration changes to all zones where the firewall is in use.
-- This summary indicates whether the configuration changes have been
-- applied everywhere.
--
-- This status must be @IN_SYNC@ for the firewall to be ready for use, but
-- it doesn\'t indicate that the firewall is ready. The @Status@ setting
-- indicates firewall readiness.
firewallStatus_configurationSyncStateSummary :: Lens.Lens' FirewallStatus ConfigurationSyncState
firewallStatus_configurationSyncStateSummary = Lens.lens (\FirewallStatus' {configurationSyncStateSummary} -> configurationSyncStateSummary) (\s@FirewallStatus' {} a -> s {configurationSyncStateSummary = a} :: FirewallStatus)

instance Data.FromJSON FirewallStatus where
  parseJSON =
    Data.withObject
      "FirewallStatus"
      ( \x ->
          FirewallStatus'
            Prelude.<$> (x Data..:? "CapacityUsageSummary")
            Prelude.<*> (x Data..:? "SyncStates" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "ConfigurationSyncStateSummary")
      )

instance Prelude.Hashable FirewallStatus where
  hashWithSalt _salt FirewallStatus' {..} =
    _salt
      `Prelude.hashWithSalt` capacityUsageSummary
      `Prelude.hashWithSalt` syncStates
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` configurationSyncStateSummary

instance Prelude.NFData FirewallStatus where
  rnf FirewallStatus' {..} =
    Prelude.rnf capacityUsageSummary `Prelude.seq`
      Prelude.rnf syncStates `Prelude.seq`
        Prelude.rnf status `Prelude.seq`
          Prelude.rnf configurationSyncStateSummary
