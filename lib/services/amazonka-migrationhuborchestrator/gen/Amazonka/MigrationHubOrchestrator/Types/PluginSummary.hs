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
-- Module      : Amazonka.MigrationHubOrchestrator.Types.PluginSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubOrchestrator.Types.PluginSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types.PluginHealth
import qualified Amazonka.Prelude as Prelude

-- | The summary of the Migration Hub Orchestrator plugin.
--
-- /See:/ 'newPluginSummary' smart constructor.
data PluginSummary = PluginSummary'
  { -- | The name of the host.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The IP address at which the plugin is located.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the plugin.
    pluginId :: Prelude.Maybe Prelude.Text,
    -- | The time at which the plugin was registered.
    registeredTime :: Prelude.Maybe Prelude.Text,
    -- | The status of the plugin.
    status :: Prelude.Maybe PluginHealth,
    -- | The version of the plugin.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PluginSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'pluginSummary_hostname' - The name of the host.
--
-- 'ipAddress', 'pluginSummary_ipAddress' - The IP address at which the plugin is located.
--
-- 'pluginId', 'pluginSummary_pluginId' - The ID of the plugin.
--
-- 'registeredTime', 'pluginSummary_registeredTime' - The time at which the plugin was registered.
--
-- 'status', 'pluginSummary_status' - The status of the plugin.
--
-- 'version', 'pluginSummary_version' - The version of the plugin.
newPluginSummary ::
  PluginSummary
newPluginSummary =
  PluginSummary'
    { hostname = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      pluginId = Prelude.Nothing,
      registeredTime = Prelude.Nothing,
      status = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the host.
pluginSummary_hostname :: Lens.Lens' PluginSummary (Prelude.Maybe Prelude.Text)
pluginSummary_hostname = Lens.lens (\PluginSummary' {hostname} -> hostname) (\s@PluginSummary' {} a -> s {hostname = a} :: PluginSummary)

-- | The IP address at which the plugin is located.
pluginSummary_ipAddress :: Lens.Lens' PluginSummary (Prelude.Maybe Prelude.Text)
pluginSummary_ipAddress = Lens.lens (\PluginSummary' {ipAddress} -> ipAddress) (\s@PluginSummary' {} a -> s {ipAddress = a} :: PluginSummary)

-- | The ID of the plugin.
pluginSummary_pluginId :: Lens.Lens' PluginSummary (Prelude.Maybe Prelude.Text)
pluginSummary_pluginId = Lens.lens (\PluginSummary' {pluginId} -> pluginId) (\s@PluginSummary' {} a -> s {pluginId = a} :: PluginSummary)

-- | The time at which the plugin was registered.
pluginSummary_registeredTime :: Lens.Lens' PluginSummary (Prelude.Maybe Prelude.Text)
pluginSummary_registeredTime = Lens.lens (\PluginSummary' {registeredTime} -> registeredTime) (\s@PluginSummary' {} a -> s {registeredTime = a} :: PluginSummary)

-- | The status of the plugin.
pluginSummary_status :: Lens.Lens' PluginSummary (Prelude.Maybe PluginHealth)
pluginSummary_status = Lens.lens (\PluginSummary' {status} -> status) (\s@PluginSummary' {} a -> s {status = a} :: PluginSummary)

-- | The version of the plugin.
pluginSummary_version :: Lens.Lens' PluginSummary (Prelude.Maybe Prelude.Text)
pluginSummary_version = Lens.lens (\PluginSummary' {version} -> version) (\s@PluginSummary' {} a -> s {version = a} :: PluginSummary)

instance Data.FromJSON PluginSummary where
  parseJSON =
    Data.withObject
      "PluginSummary"
      ( \x ->
          PluginSummary'
            Prelude.<$> (x Data..:? "hostname")
            Prelude.<*> (x Data..:? "ipAddress")
            Prelude.<*> (x Data..:? "pluginId")
            Prelude.<*> (x Data..:? "registeredTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "version")
      )

instance Prelude.Hashable PluginSummary where
  hashWithSalt _salt PluginSummary' {..} =
    _salt `Prelude.hashWithSalt` hostname
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` pluginId
      `Prelude.hashWithSalt` registeredTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` version

instance Prelude.NFData PluginSummary where
  rnf PluginSummary' {..} =
    Prelude.rnf hostname
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf pluginId
      `Prelude.seq` Prelude.rnf registeredTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
