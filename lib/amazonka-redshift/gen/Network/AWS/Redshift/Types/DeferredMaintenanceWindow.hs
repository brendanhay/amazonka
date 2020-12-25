{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DeferredMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DeferredMaintenanceWindow
  ( DeferredMaintenanceWindow (..),

    -- * Smart constructor
    mkDeferredMaintenanceWindow,

    -- * Lenses
    dmwDeferMaintenanceEndTime,
    dmwDeferMaintenanceIdentifier,
    dmwDeferMaintenanceStartTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.String as Types

-- | Describes a deferred maintenance window
--
-- /See:/ 'mkDeferredMaintenanceWindow' smart constructor.
data DeferredMaintenanceWindow = DeferredMaintenanceWindow'
  { -- | A timestamp for the end of the time period when we defer maintenance.
    deferMaintenanceEndTime :: Core.Maybe Core.UTCTime,
    -- | A unique identifier for the maintenance window.
    deferMaintenanceIdentifier :: Core.Maybe Types.String,
    -- | A timestamp for the beginning of the time period when we defer maintenance.
    deferMaintenanceStartTime :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DeferredMaintenanceWindow' value with any optional fields omitted.
mkDeferredMaintenanceWindow ::
  DeferredMaintenanceWindow
mkDeferredMaintenanceWindow =
  DeferredMaintenanceWindow'
    { deferMaintenanceEndTime =
        Core.Nothing,
      deferMaintenanceIdentifier = Core.Nothing,
      deferMaintenanceStartTime = Core.Nothing
    }

-- | A timestamp for the end of the time period when we defer maintenance.
--
-- /Note:/ Consider using 'deferMaintenanceEndTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwDeferMaintenanceEndTime :: Lens.Lens' DeferredMaintenanceWindow (Core.Maybe Core.UTCTime)
dmwDeferMaintenanceEndTime = Lens.field @"deferMaintenanceEndTime"
{-# DEPRECATED dmwDeferMaintenanceEndTime "Use generic-lens or generic-optics with 'deferMaintenanceEndTime' instead." #-}

-- | A unique identifier for the maintenance window.
--
-- /Note:/ Consider using 'deferMaintenanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwDeferMaintenanceIdentifier :: Lens.Lens' DeferredMaintenanceWindow (Core.Maybe Types.String)
dmwDeferMaintenanceIdentifier = Lens.field @"deferMaintenanceIdentifier"
{-# DEPRECATED dmwDeferMaintenanceIdentifier "Use generic-lens or generic-optics with 'deferMaintenanceIdentifier' instead." #-}

-- | A timestamp for the beginning of the time period when we defer maintenance.
--
-- /Note:/ Consider using 'deferMaintenanceStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwDeferMaintenanceStartTime :: Lens.Lens' DeferredMaintenanceWindow (Core.Maybe Core.UTCTime)
dmwDeferMaintenanceStartTime = Lens.field @"deferMaintenanceStartTime"
{-# DEPRECATED dmwDeferMaintenanceStartTime "Use generic-lens or generic-optics with 'deferMaintenanceStartTime' instead." #-}

instance Core.FromXML DeferredMaintenanceWindow where
  parseXML x =
    DeferredMaintenanceWindow'
      Core.<$> (x Core..@? "DeferMaintenanceEndTime")
      Core.<*> (x Core..@? "DeferMaintenanceIdentifier")
      Core.<*> (x Core..@? "DeferMaintenanceStartTime")
