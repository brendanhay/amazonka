{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ResourcePendingMaintenanceActions
  ( ResourcePendingMaintenanceActions (..),

    -- * Smart constructor
    mkResourcePendingMaintenanceActions,

    -- * Lenses
    rpmaPendingMaintenanceActionDetails,
    rpmaResourceIdentifier,
  )
where

import qualified Network.AWS.DMS.Types.PendingMaintenanceAction as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies an AWS DMS resource and any pending actions for it.
--
-- /See:/ 'mkResourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { -- | Detailed information about the pending maintenance action.
    pendingMaintenanceActionDetails :: Core.Maybe [Types.PendingMaintenanceAction],
    -- | The Amazon Resource Name (ARN) of the DMS resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS> in the DMS documentation.
    resourceIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ResourcePendingMaintenanceActions' value with any optional fields omitted.
mkResourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
mkResourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { pendingMaintenanceActionDetails =
        Core.Nothing,
      resourceIdentifier = Core.Nothing
    }

-- | Detailed information about the pending maintenance action.
--
-- /Note:/ Consider using 'pendingMaintenanceActionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmaPendingMaintenanceActionDetails :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe [Types.PendingMaintenanceAction])
rpmaPendingMaintenanceActionDetails = Lens.field @"pendingMaintenanceActionDetails"
{-# DEPRECATED rpmaPendingMaintenanceActionDetails "Use generic-lens or generic-optics with 'pendingMaintenanceActionDetails' instead." #-}

-- | The Amazon Resource Name (ARN) of the DMS resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS> in the DMS documentation.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmaResourceIdentifier :: Lens.Lens' ResourcePendingMaintenanceActions (Core.Maybe Types.String)
rpmaResourceIdentifier = Lens.field @"resourceIdentifier"
{-# DEPRECATED rpmaResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Core.FromJSON ResourcePendingMaintenanceActions where
  parseJSON =
    Core.withObject "ResourcePendingMaintenanceActions" Core.$
      \x ->
        ResourcePendingMaintenanceActions'
          Core.<$> (x Core..:? "PendingMaintenanceActionDetails")
          Core.<*> (x Core..:? "ResourceIdentifier")
