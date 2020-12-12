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

import Network.AWS.DMS.Types.PendingMaintenanceAction
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies an AWS DMS resource and any pending actions for it.
--
-- /See:/ 'mkResourcePendingMaintenanceActions' smart constructor.
data ResourcePendingMaintenanceActions = ResourcePendingMaintenanceActions'
  { pendingMaintenanceActionDetails ::
      Lude.Maybe
        [PendingMaintenanceAction],
    resourceIdentifier ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResourcePendingMaintenanceActions' with the minimum fields required to make a request.
--
-- * 'pendingMaintenanceActionDetails' - Detailed information about the pending maintenance action.
-- * 'resourceIdentifier' - The Amazon Resource Name (ARN) of the DMS resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS> in the DMS documentation.
mkResourcePendingMaintenanceActions ::
  ResourcePendingMaintenanceActions
mkResourcePendingMaintenanceActions =
  ResourcePendingMaintenanceActions'
    { pendingMaintenanceActionDetails =
        Lude.Nothing,
      resourceIdentifier = Lude.Nothing
    }

-- | Detailed information about the pending maintenance action.
--
-- /Note:/ Consider using 'pendingMaintenanceActionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmaPendingMaintenanceActionDetails :: Lens.Lens' ResourcePendingMaintenanceActions (Lude.Maybe [PendingMaintenanceAction])
rpmaPendingMaintenanceActionDetails = Lens.lens (pendingMaintenanceActionDetails :: ResourcePendingMaintenanceActions -> Lude.Maybe [PendingMaintenanceAction]) (\s a -> s {pendingMaintenanceActionDetails = a} :: ResourcePendingMaintenanceActions)
{-# DEPRECATED rpmaPendingMaintenanceActionDetails "Use generic-lens or generic-optics with 'pendingMaintenanceActionDetails' instead." #-}

-- | The Amazon Resource Name (ARN) of the DMS resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Introduction.AWS.ARN.html Constructing an Amazon Resource Name (ARN) for AWS DMS> in the DMS documentation.
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpmaResourceIdentifier :: Lens.Lens' ResourcePendingMaintenanceActions (Lude.Maybe Lude.Text)
rpmaResourceIdentifier = Lens.lens (resourceIdentifier :: ResourcePendingMaintenanceActions -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdentifier = a} :: ResourcePendingMaintenanceActions)
{-# DEPRECATED rpmaResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

instance Lude.FromJSON ResourcePendingMaintenanceActions where
  parseJSON =
    Lude.withObject
      "ResourcePendingMaintenanceActions"
      ( \x ->
          ResourcePendingMaintenanceActions'
            Lude.<$> (x Lude..:? "PendingMaintenanceActionDetails" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ResourceIdentifier")
      )
