{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.DeletionTaskFailureReasonType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.DeletionTaskFailureReasonType
  ( DeletionTaskFailureReasonType (..),

    -- * Smart constructor
    mkDeletionTaskFailureReasonType,

    -- * Lenses
    dtfrtRoleUsageList,
    dtfrtReason,
  )
where

import Network.AWS.IAM.Types.RoleUsageType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The reason that the service-linked role deletion failed.
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
-- /See:/ 'mkDeletionTaskFailureReasonType' smart constructor.
data DeletionTaskFailureReasonType = DeletionTaskFailureReasonType'
  { roleUsageList ::
      Lude.Maybe [RoleUsageType],
    reason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletionTaskFailureReasonType' with the minimum fields required to make a request.
--
-- * 'reason' - A short description of the reason that the service-linked role deletion failed.
-- * 'roleUsageList' - A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the Region in which the resources are being used.
mkDeletionTaskFailureReasonType ::
  DeletionTaskFailureReasonType
mkDeletionTaskFailureReasonType =
  DeletionTaskFailureReasonType'
    { roleUsageList = Lude.Nothing,
      reason = Lude.Nothing
    }

-- | A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the Region in which the resources are being used.
--
-- /Note:/ Consider using 'roleUsageList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrtRoleUsageList :: Lens.Lens' DeletionTaskFailureReasonType (Lude.Maybe [RoleUsageType])
dtfrtRoleUsageList = Lens.lens (roleUsageList :: DeletionTaskFailureReasonType -> Lude.Maybe [RoleUsageType]) (\s a -> s {roleUsageList = a} :: DeletionTaskFailureReasonType)
{-# DEPRECATED dtfrtRoleUsageList "Use generic-lens or generic-optics with 'roleUsageList' instead." #-}

-- | A short description of the reason that the service-linked role deletion failed.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrtReason :: Lens.Lens' DeletionTaskFailureReasonType (Lude.Maybe Lude.Text)
dtfrtReason = Lens.lens (reason :: DeletionTaskFailureReasonType -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: DeletionTaskFailureReasonType)
{-# DEPRECATED dtfrtReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Lude.FromXML DeletionTaskFailureReasonType where
  parseXML x =
    DeletionTaskFailureReasonType'
      Lude.<$> ( x Lude..@? "RoleUsageList" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Reason")
