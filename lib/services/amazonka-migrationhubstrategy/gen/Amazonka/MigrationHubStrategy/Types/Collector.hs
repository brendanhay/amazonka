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
-- Module      : Amazonka.MigrationHubStrategy.Types.Collector
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Collector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types.CollectorHealth
import Amazonka.MigrationHubStrategy.Types.ConfigurationSummary
import qualified Amazonka.Prelude as Prelude

-- | Process data collector that runs in the environment that you specify.
--
-- /See:/ 'newCollector' smart constructor.
data Collector = Collector'
  { -- | Indicates the health of a collector.
    collectorHealth :: Prelude.Maybe CollectorHealth,
    -- | The ID of the collector.
    collectorId :: Prelude.Maybe Prelude.Text,
    -- | Current version of the collector that is running in the environment that
    -- you specify.
    collectorVersion :: Prelude.Maybe Prelude.Text,
    -- | Summary of the collector configuration.
    configurationSummary :: Prelude.Maybe ConfigurationSummary,
    -- | Hostname of the server that is hosting the collector.
    hostName :: Prelude.Maybe Prelude.Text,
    -- | IP address of the server that is hosting the collector.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | Time when the collector last pinged the service.
    lastActivityTimeStamp :: Prelude.Maybe Prelude.Text,
    -- | Time when the collector registered with the service.
    registeredTimeStamp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Collector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectorHealth', 'collector_collectorHealth' - Indicates the health of a collector.
--
-- 'collectorId', 'collector_collectorId' - The ID of the collector.
--
-- 'collectorVersion', 'collector_collectorVersion' - Current version of the collector that is running in the environment that
-- you specify.
--
-- 'configurationSummary', 'collector_configurationSummary' - Summary of the collector configuration.
--
-- 'hostName', 'collector_hostName' - Hostname of the server that is hosting the collector.
--
-- 'ipAddress', 'collector_ipAddress' - IP address of the server that is hosting the collector.
--
-- 'lastActivityTimeStamp', 'collector_lastActivityTimeStamp' - Time when the collector last pinged the service.
--
-- 'registeredTimeStamp', 'collector_registeredTimeStamp' - Time when the collector registered with the service.
newCollector ::
  Collector
newCollector =
  Collector'
    { collectorHealth = Prelude.Nothing,
      collectorId = Prelude.Nothing,
      collectorVersion = Prelude.Nothing,
      configurationSummary = Prelude.Nothing,
      hostName = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      lastActivityTimeStamp = Prelude.Nothing,
      registeredTimeStamp = Prelude.Nothing
    }

-- | Indicates the health of a collector.
collector_collectorHealth :: Lens.Lens' Collector (Prelude.Maybe CollectorHealth)
collector_collectorHealth = Lens.lens (\Collector' {collectorHealth} -> collectorHealth) (\s@Collector' {} a -> s {collectorHealth = a} :: Collector)

-- | The ID of the collector.
collector_collectorId :: Lens.Lens' Collector (Prelude.Maybe Prelude.Text)
collector_collectorId = Lens.lens (\Collector' {collectorId} -> collectorId) (\s@Collector' {} a -> s {collectorId = a} :: Collector)

-- | Current version of the collector that is running in the environment that
-- you specify.
collector_collectorVersion :: Lens.Lens' Collector (Prelude.Maybe Prelude.Text)
collector_collectorVersion = Lens.lens (\Collector' {collectorVersion} -> collectorVersion) (\s@Collector' {} a -> s {collectorVersion = a} :: Collector)

-- | Summary of the collector configuration.
collector_configurationSummary :: Lens.Lens' Collector (Prelude.Maybe ConfigurationSummary)
collector_configurationSummary = Lens.lens (\Collector' {configurationSummary} -> configurationSummary) (\s@Collector' {} a -> s {configurationSummary = a} :: Collector)

-- | Hostname of the server that is hosting the collector.
collector_hostName :: Lens.Lens' Collector (Prelude.Maybe Prelude.Text)
collector_hostName = Lens.lens (\Collector' {hostName} -> hostName) (\s@Collector' {} a -> s {hostName = a} :: Collector)

-- | IP address of the server that is hosting the collector.
collector_ipAddress :: Lens.Lens' Collector (Prelude.Maybe Prelude.Text)
collector_ipAddress = Lens.lens (\Collector' {ipAddress} -> ipAddress) (\s@Collector' {} a -> s {ipAddress = a} :: Collector)

-- | Time when the collector last pinged the service.
collector_lastActivityTimeStamp :: Lens.Lens' Collector (Prelude.Maybe Prelude.Text)
collector_lastActivityTimeStamp = Lens.lens (\Collector' {lastActivityTimeStamp} -> lastActivityTimeStamp) (\s@Collector' {} a -> s {lastActivityTimeStamp = a} :: Collector)

-- | Time when the collector registered with the service.
collector_registeredTimeStamp :: Lens.Lens' Collector (Prelude.Maybe Prelude.Text)
collector_registeredTimeStamp = Lens.lens (\Collector' {registeredTimeStamp} -> registeredTimeStamp) (\s@Collector' {} a -> s {registeredTimeStamp = a} :: Collector)

instance Data.FromJSON Collector where
  parseJSON =
    Data.withObject
      "Collector"
      ( \x ->
          Collector'
            Prelude.<$> (x Data..:? "collectorHealth")
            Prelude.<*> (x Data..:? "collectorId")
            Prelude.<*> (x Data..:? "collectorVersion")
            Prelude.<*> (x Data..:? "configurationSummary")
            Prelude.<*> (x Data..:? "hostName")
            Prelude.<*> (x Data..:? "ipAddress")
            Prelude.<*> (x Data..:? "lastActivityTimeStamp")
            Prelude.<*> (x Data..:? "registeredTimeStamp")
      )

instance Prelude.Hashable Collector where
  hashWithSalt _salt Collector' {..} =
    _salt
      `Prelude.hashWithSalt` collectorHealth
      `Prelude.hashWithSalt` collectorId
      `Prelude.hashWithSalt` collectorVersion
      `Prelude.hashWithSalt` configurationSummary
      `Prelude.hashWithSalt` hostName
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` lastActivityTimeStamp
      `Prelude.hashWithSalt` registeredTimeStamp

instance Prelude.NFData Collector where
  rnf Collector' {..} =
    Prelude.rnf collectorHealth
      `Prelude.seq` Prelude.rnf collectorId
      `Prelude.seq` Prelude.rnf collectorVersion
      `Prelude.seq` Prelude.rnf configurationSummary
      `Prelude.seq` Prelude.rnf hostName
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf lastActivityTimeStamp
      `Prelude.seq` Prelude.rnf registeredTimeStamp
