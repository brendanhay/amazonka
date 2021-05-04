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
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfiguration where

import Network.AWS.Greengrass.Types.ConfigurationSyncStatus
import Network.AWS.Greengrass.Types.Telemetry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'newTelemetryConfiguration' smart constructor.
data TelemetryConfiguration = TelemetryConfiguration'
  { -- | Synchronization status of the device reported configuration with the
    -- desired configuration.
    configurationSyncStatus :: Prelude.Maybe ConfigurationSyncStatus,
    -- | Configure telemetry to be on or off.
    telemetry :: Telemetry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      telemetry = pTelemetry_
    }

-- | Synchronization status of the device reported configuration with the
-- desired configuration.
telemetryConfiguration_configurationSyncStatus :: Lens.Lens' TelemetryConfiguration (Prelude.Maybe ConfigurationSyncStatus)
telemetryConfiguration_configurationSyncStatus = Lens.lens (\TelemetryConfiguration' {configurationSyncStatus} -> configurationSyncStatus) (\s@TelemetryConfiguration' {} a -> s {configurationSyncStatus = a} :: TelemetryConfiguration)

-- | Configure telemetry to be on or off.
telemetryConfiguration_telemetry :: Lens.Lens' TelemetryConfiguration Telemetry
telemetryConfiguration_telemetry = Lens.lens (\TelemetryConfiguration' {telemetry} -> telemetry) (\s@TelemetryConfiguration' {} a -> s {telemetry = a} :: TelemetryConfiguration)

instance Prelude.FromJSON TelemetryConfiguration where
  parseJSON =
    Prelude.withObject
      "TelemetryConfiguration"
      ( \x ->
          TelemetryConfiguration'
            Prelude.<$> (x Prelude..:? "ConfigurationSyncStatus")
            Prelude.<*> (x Prelude..: "Telemetry")
      )

instance Prelude.Hashable TelemetryConfiguration

instance Prelude.NFData TelemetryConfiguration
