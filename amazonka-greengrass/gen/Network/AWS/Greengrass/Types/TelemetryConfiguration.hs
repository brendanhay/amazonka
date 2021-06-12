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
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types.ConfigurationSyncStatus
import Network.AWS.Greengrass.Types.Telemetry
import qualified Network.AWS.Lens as Lens

-- | Configuration settings for running telemetry.
--
-- /See:/ 'newTelemetryConfiguration' smart constructor.
data TelemetryConfiguration = TelemetryConfiguration'
  { -- | Synchronization status of the device reported configuration with the
    -- desired configuration.
    configurationSyncStatus :: Core.Maybe ConfigurationSyncStatus,
    -- | Configure telemetry to be on or off.
    telemetry :: Telemetry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TelemetryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSyncStatus', 'telemetryConfiguration_configurationSyncStatus' - Synchronization status of the device reported configuration with the
-- desired configuration.
--
-- 'telemetry', 'telemetryConfiguration_telemetry' - Configure telemetry to be on or off.
newTelemetryConfiguration ::
  -- | 'telemetry'
  Telemetry ->
  TelemetryConfiguration
newTelemetryConfiguration pTelemetry_ =
  TelemetryConfiguration'
    { configurationSyncStatus =
        Core.Nothing,
      telemetry = pTelemetry_
    }

-- | Synchronization status of the device reported configuration with the
-- desired configuration.
telemetryConfiguration_configurationSyncStatus :: Lens.Lens' TelemetryConfiguration (Core.Maybe ConfigurationSyncStatus)
telemetryConfiguration_configurationSyncStatus = Lens.lens (\TelemetryConfiguration' {configurationSyncStatus} -> configurationSyncStatus) (\s@TelemetryConfiguration' {} a -> s {configurationSyncStatus = a} :: TelemetryConfiguration)

-- | Configure telemetry to be on or off.
telemetryConfiguration_telemetry :: Lens.Lens' TelemetryConfiguration Telemetry
telemetryConfiguration_telemetry = Lens.lens (\TelemetryConfiguration' {telemetry} -> telemetry) (\s@TelemetryConfiguration' {} a -> s {telemetry = a} :: TelemetryConfiguration)

instance Core.FromJSON TelemetryConfiguration where
  parseJSON =
    Core.withObject
      "TelemetryConfiguration"
      ( \x ->
          TelemetryConfiguration'
            Core.<$> (x Core..:? "ConfigurationSyncStatus")
            Core.<*> (x Core..: "Telemetry")
      )

instance Core.Hashable TelemetryConfiguration

instance Core.NFData TelemetryConfiguration
