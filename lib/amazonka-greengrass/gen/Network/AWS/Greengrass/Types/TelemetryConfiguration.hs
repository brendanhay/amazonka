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
    tcConfigurationSyncStatus,
    tcTelemetry,
  )
where

import Network.AWS.Greengrass.Types.ConfigurationSyncStatus
import Network.AWS.Greengrass.Types.Telemetry
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configuration settings for running telemetry.
--
-- /See:/ 'mkTelemetryConfiguration' smart constructor.
data TelemetryConfiguration = TelemetryConfiguration'
  { configurationSyncStatus ::
      Lude.Maybe ConfigurationSyncStatus,
    telemetry :: Telemetry
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TelemetryConfiguration' with the minimum fields required to make a request.
--
-- * 'configurationSyncStatus' - Synchronization status of the device reported configuration with the desired configuration.
-- * 'telemetry' - Configure telemetry to be on or off.
mkTelemetryConfiguration ::
  -- | 'telemetry'
  Telemetry ->
  TelemetryConfiguration
mkTelemetryConfiguration pTelemetry_ =
  TelemetryConfiguration'
    { configurationSyncStatus = Lude.Nothing,
      telemetry = pTelemetry_
    }

-- | Synchronization status of the device reported configuration with the desired configuration.
--
-- /Note:/ Consider using 'configurationSyncStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcConfigurationSyncStatus :: Lens.Lens' TelemetryConfiguration (Lude.Maybe ConfigurationSyncStatus)
tcConfigurationSyncStatus = Lens.lens (configurationSyncStatus :: TelemetryConfiguration -> Lude.Maybe ConfigurationSyncStatus) (\s a -> s {configurationSyncStatus = a} :: TelemetryConfiguration)
{-# DEPRECATED tcConfigurationSyncStatus "Use generic-lens or generic-optics with 'configurationSyncStatus' instead." #-}

-- | Configure telemetry to be on or off.
--
-- /Note:/ Consider using 'telemetry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcTelemetry :: Lens.Lens' TelemetryConfiguration Telemetry
tcTelemetry = Lens.lens (telemetry :: TelemetryConfiguration -> Telemetry) (\s a -> s {telemetry = a} :: TelemetryConfiguration)
{-# DEPRECATED tcTelemetry "Use generic-lens or generic-optics with 'telemetry' instead." #-}

instance Lude.FromJSON TelemetryConfiguration where
  parseJSON =
    Lude.withObject
      "TelemetryConfiguration"
      ( \x ->
          TelemetryConfiguration'
            Lude.<$> (x Lude..:? "ConfigurationSyncStatus")
            Lude.<*> (x Lude..: "Telemetry")
      )
