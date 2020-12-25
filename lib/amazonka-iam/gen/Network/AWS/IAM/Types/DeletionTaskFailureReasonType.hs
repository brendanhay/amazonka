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
    dtfrtReason,
    dtfrtRoleUsageList,
  )
where

import qualified Network.AWS.IAM.Types.ReasonType as Types
import qualified Network.AWS.IAM.Types.RoleUsageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The reason that the service-linked role deletion failed.
--
-- This data type is used as a response element in the 'GetServiceLinkedRoleDeletionStatus' operation.
--
-- /See:/ 'mkDeletionTaskFailureReasonType' smart constructor.
data DeletionTaskFailureReasonType = DeletionTaskFailureReasonType'
  { -- | A short description of the reason that the service-linked role deletion failed.
    reason :: Core.Maybe Types.ReasonType,
    -- | A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the Region in which the resources are being used.
    roleUsageList :: Core.Maybe [Types.RoleUsageType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeletionTaskFailureReasonType' value with any optional fields omitted.
mkDeletionTaskFailureReasonType ::
  DeletionTaskFailureReasonType
mkDeletionTaskFailureReasonType =
  DeletionTaskFailureReasonType'
    { reason = Core.Nothing,
      roleUsageList = Core.Nothing
    }

-- | A short description of the reason that the service-linked role deletion failed.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrtReason :: Lens.Lens' DeletionTaskFailureReasonType (Core.Maybe Types.ReasonType)
dtfrtReason = Lens.field @"reason"
{-# DEPRECATED dtfrtReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | A list of objects that contains details about the service-linked role deletion failure, if that information is returned by the service. If the service-linked role has active sessions or if any resources that were used by the role have not been deleted from the linked service, the role can't be deleted. This parameter includes a list of the resources that are associated with the role and the Region in which the resources are being used.
--
-- /Note:/ Consider using 'roleUsageList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtfrtRoleUsageList :: Lens.Lens' DeletionTaskFailureReasonType (Core.Maybe [Types.RoleUsageType])
dtfrtRoleUsageList = Lens.field @"roleUsageList"
{-# DEPRECATED dtfrtRoleUsageList "Use generic-lens or generic-optics with 'roleUsageList' instead." #-}

instance Core.FromXML DeletionTaskFailureReasonType where
  parseXML x =
    DeletionTaskFailureReasonType'
      Core.<$> (x Core..@? "Reason")
      Core.<*> (x Core..@? "RoleUsageList" Core..<@> Core.parseXMLList "member")
