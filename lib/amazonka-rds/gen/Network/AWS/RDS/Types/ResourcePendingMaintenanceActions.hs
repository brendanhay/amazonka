{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.ResourcePendingMaintenanceActions
  ( ResourcePendingMaintenanceActions (..)
  -- * Smart constructor
  , mkResourcePendingMaintenanceActions
  -- * Lenses
  , rpmaPendingMaintenanceActionDetails
  , rpmaResourceIdentifier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types.PendingMaintenanceAction as Types

-- | Describes the pending maintenance actions for a resource.
--
-- /See:/ 'mkResourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { pendingMaintenanceActionDetails :: Core.Maybe [Types.PendingMaintenanceAction]
    -- ^ A list that provides details about the pending maintenance actions for the resource.
  , resourceIdentifier :: Core.Maybe Core.Text
    -- ^ The ARN of the resource that has pending maintenance actions.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ResourcePendingMaintenanceActions' value with any optional fields omitted.
mkResourcePendingMaintenanceActions
    :: ResourcePendingMaintenanceActions
mkResourcePendingMaintenanceActions
  = ResourcePendingMaintenanceActions'{pendingMaintenanceActionDetails
                                         = Core.Nothing,
                                       resourceIdentifier = Core.Nothing}

-- | A list that provides details about the pending maintenance actions for the resource.
--
-- /Note:/ Consider using 'pendingMaintenanceActionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmaPendingMaintenanceActionDetails :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe [Types.PendingMaintenanceAction])
rpmaPendingMaintenanceActionDetails = Lens.field @"pendingMaintenanceActionDetails"
{-# INLINEABLE rpmaPendingMaintenanceActionDetails #-}
{-# DEPRECATED pendingMaintenanceActionDetails "Use generic-lens or generic-optics with 'pendingMaintenanceActionDetails' instead"  #-}

-- | The ARN of the resource that has pending maintenance actions.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmaResourceIdentifier :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe Core.Text)
rpmaResourceIdentifier = Lens.field @"resourceIdentifier"
{-# INLINEABLE rpmaResourceIdentifier #-}
{-# DEPRECATED resourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead"  #-}

instance Core.FromXML ResourcePendingMaintenanceActions where
        parseXML x
          = ResourcePendingMaintenanceActions' Core.<$>
              (x Core..@? "PendingMaintenanceActionDetails" Core..<@>
                 Core.parseXMLList "PendingMaintenanceAction")
                Core.<*> x Core..@? "ResourceIdentifier"
