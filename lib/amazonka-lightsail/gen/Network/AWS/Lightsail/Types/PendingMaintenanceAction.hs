{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.PendingMaintenanceAction
  ( PendingMaintenanceAction (..)
  -- * Smart constructor
  , mkPendingMaintenanceAction
  -- * Lenses
  , pmaAction
  , pmaCurrentApplyDate
  , pmaDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.Action as Types
import qualified Network.AWS.Lightsail.Types.Description as Types
import qualified Network.AWS.Prelude as Core

-- | Describes a pending database maintenance action.
--
-- /See:/ 'mkPendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { action :: Core.Maybe Types.Action
    -- ^ The type of pending database maintenance action.
  , currentApplyDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The effective date of the pending database maintenance action.
  , description :: Core.Maybe Types.Description
    -- ^ Additional detail about the pending database maintenance action.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PendingMaintenanceAction' value with any optional fields omitted.
mkPendingMaintenanceAction
    :: PendingMaintenanceAction
mkPendingMaintenanceAction
  = PendingMaintenanceAction'{action = Core.Nothing,
                              currentApplyDate = Core.Nothing, description = Core.Nothing}

-- | The type of pending database maintenance action.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaAction :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Types.Action)
pmaAction = Lens.field @"action"
{-# INLINEABLE pmaAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The effective date of the pending database maintenance action.
--
-- /Note:/ Consider using 'currentApplyDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaCurrentApplyDate :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Core.NominalDiffTime)
pmaCurrentApplyDate = Lens.field @"currentApplyDate"
{-# INLINEABLE pmaCurrentApplyDate #-}
{-# DEPRECATED currentApplyDate "Use generic-lens or generic-optics with 'currentApplyDate' instead"  #-}

-- | Additional detail about the pending database maintenance action.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pmaDescription :: Lens.Lens' PendingMaintenanceAction (Core.Maybe Types.Description)
pmaDescription = Lens.field @"description"
{-# INLINEABLE pmaDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON PendingMaintenanceAction where
        parseJSON
          = Core.withObject "PendingMaintenanceAction" Core.$
              \ x ->
                PendingMaintenanceAction' Core.<$>
                  (x Core..:? "action") Core.<*> x Core..:? "currentApplyDate"
                    Core.<*> x Core..:? "description"
