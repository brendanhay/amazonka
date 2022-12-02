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
-- Module      : Amazonka.Greengrass.Types.TelemetryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.TelemetryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types.ConfigurationSyncStatus
import Amazonka.Greengrass.Types.Telemetry
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON TelemetryConfiguration where
  parseJSON =
    Data.withObject
      "TelemetryConfiguration"
      ( \x ->
          TelemetryConfiguration'
            Prelude.<$> (x Data..:? "ConfigurationSyncStatus")
            Prelude.<*> (x Data..: "Telemetry")
      )

instance Prelude.Hashable TelemetryConfiguration where
  hashWithSalt _salt TelemetryConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` configurationSyncStatus
      `Prelude.hashWithSalt` telemetry

instance Prelude.NFData TelemetryConfiguration where
  rnf TelemetryConfiguration' {..} =
    Prelude.rnf configurationSyncStatus
      `Prelude.seq` Prelude.rnf telemetry
