{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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

import qualified Network.AWS.Greengrass.Types.TelemetryConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Runtime configuration for a thing.
--
-- /See:/ 'mkRuntimeConfiguration' smart constructor.
newtype RuntimeConfiguration = RuntimeConfiguration'
  { -- | Configuration for telemetry service.
    telemetryConfiguration :: Core.Maybe Types.TelemetryConfiguration
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RuntimeConfiguration' value with any optional fields omitted.
mkRuntimeConfiguration ::
  RuntimeConfiguration
mkRuntimeConfiguration =
  RuntimeConfiguration' {telemetryConfiguration = Core.Nothing}

-- | Configuration for telemetry service.
--
-- /Note:/ Consider using 'telemetryConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcTelemetryConfiguration :: Lens.Lens' RuntimeConfiguration (Core.Maybe Types.TelemetryConfiguration)
rcTelemetryConfiguration = Lens.field @"telemetryConfiguration"
{-# DEPRECATED rcTelemetryConfiguration "Use generic-lens or generic-optics with 'telemetryConfiguration' instead." #-}

instance Core.FromJSON RuntimeConfiguration where
  parseJSON =
    Core.withObject "RuntimeConfiguration" Core.$
      \x ->
        RuntimeConfiguration'
          Core.<$> (x Core..:? "TelemetryConfiguration")
