{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfiguration
  ( TelemetryConfiguration (..),

    -- * Smart constructor
    mkTelemetryConfiguration,

    -- * Lenses
    tcTelemetry,
    tcConfigurationSyncStatus,
  )
where

import qualified Network.AWS.Greengrass.Types.ConfigurationSyncStatus as Types
import qualified Network.AWS.Greengrass.Types.Telemetry as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration settings for running telemetry.
--
-- /See:/ 'mkTelemetryConfiguration' smart constructor.
data TelemetryConfiguration = TelemetryConfiguration'
  { -- | Configure telemetry to be on or off.
    telemetry :: Types.Telemetry,
    -- | Synchronization status of the device reported configuration with the desired configuration.
    configurationSyncStatus :: Core.Maybe Types.ConfigurationSyncStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TelemetryConfiguration' value with any optional fields omitted.
mkTelemetryConfiguration ::
  -- | 'telemetry'
  Types.Telemetry ->
  TelemetryConfiguration
mkTelemetryConfiguration telemetry =
  TelemetryConfiguration'
    { telemetry,
      configurationSyncStatus = Core.Nothing
    }

-- | Configure telemetry to be on or off.
--
-- /Note:/ Consider using 'telemetry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTelemetry :: Lens.Lens' TelemetryConfiguration Types.Telemetry
tcTelemetry = Lens.field @"telemetry"
{-# DEPRECATED tcTelemetry "Use generic-lens or generic-optics with 'telemetry' instead." #-}

-- | Synchronization status of the device reported configuration with the desired configuration.
--
-- /Note:/ Consider using 'configurationSyncStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcConfigurationSyncStatus :: Lens.Lens' TelemetryConfiguration (Core.Maybe Types.ConfigurationSyncStatus)
tcConfigurationSyncStatus = Lens.field @"configurationSyncStatus"
{-# DEPRECATED tcConfigurationSyncStatus "Use generic-lens or generic-optics with 'configurationSyncStatus' instead." #-}

instance Core.FromJSON TelemetryConfiguration where
  parseJSON =
    Core.withObject "TelemetryConfiguration" Core.$
      \x ->
        TelemetryConfiguration'
          Core.<$> (x Core..: "Telemetry")
          Core.<*> (x Core..:? "ConfigurationSyncStatus")
