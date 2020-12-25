{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.TelemetryConfigurationUpdate
  ( TelemetryConfigurationUpdate (..),

    -- * Smart constructor
    mkTelemetryConfigurationUpdate,

    -- * Lenses
    tcuTelemetry,
  )
where

import qualified Network.AWS.Greengrass.Types.Telemetry as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Configuration settings for running telemetry.
--
-- /See:/ 'mkTelemetryConfigurationUpdate' smart constructor.
newtype TelemetryConfigurationUpdate = TelemetryConfigurationUpdate'
  { -- | Configure telemetry to be on or off.
    telemetry :: Types.Telemetry
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'TelemetryConfigurationUpdate' value with any optional fields omitted.
mkTelemetryConfigurationUpdate ::
  -- | 'telemetry'
  Types.Telemetry ->
  TelemetryConfigurationUpdate
mkTelemetryConfigurationUpdate telemetry =
  TelemetryConfigurationUpdate' {telemetry}

-- | Configure telemetry to be on or off.
--
-- /Note:/ Consider using 'telemetry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcuTelemetry :: Lens.Lens' TelemetryConfigurationUpdate Types.Telemetry
tcuTelemetry = Lens.field @"telemetry"
{-# DEPRECATED tcuTelemetry "Use generic-lens or generic-optics with 'telemetry' instead." #-}

instance Core.FromJSON TelemetryConfigurationUpdate where
  toJSON TelemetryConfigurationUpdate {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Telemetry" Core..= telemetry)])
