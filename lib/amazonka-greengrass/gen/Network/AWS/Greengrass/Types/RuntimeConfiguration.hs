-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.RuntimeConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.RuntimeConfiguration
  ( RuntimeConfiguration (..),

    -- * Smart constructor
    mkRuntimeConfiguration,

    -- * Lenses
    rcTelemetryConfiguration,
  )
where

import Network.AWS.Greengrass.Types.TelemetryConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Runtime configuration for a thing.
--
-- /See:/ 'mkRuntimeConfiguration' smart constructor.
newtype RuntimeConfiguration = RuntimeConfiguration'
  { telemetryConfiguration ::
      Lude.Maybe TelemetryConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RuntimeConfiguration' with the minimum fields required to make a request.
--
-- * 'telemetryConfiguration' - Configuration for telemetry service.
mkRuntimeConfiguration ::
  RuntimeConfiguration
mkRuntimeConfiguration =
  RuntimeConfiguration' {telemetryConfiguration = Lude.Nothing}

-- | Configuration for telemetry service.
--
-- /Note:/ Consider using 'telemetryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTelemetryConfiguration :: Lens.Lens' RuntimeConfiguration (Lude.Maybe TelemetryConfiguration)
rcTelemetryConfiguration = Lens.lens (telemetryConfiguration :: RuntimeConfiguration -> Lude.Maybe TelemetryConfiguration) (\s a -> s {telemetryConfiguration = a} :: RuntimeConfiguration)
{-# DEPRECATED rcTelemetryConfiguration "Use generic-lens or generic-optics with 'telemetryConfiguration' instead." #-}

instance Lude.FromJSON RuntimeConfiguration where
  parseJSON =
    Lude.withObject
      "RuntimeConfiguration"
      ( \x ->
          RuntimeConfiguration'
            Lude.<$> (x Lude..:? "TelemetryConfiguration")
      )
