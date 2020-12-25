{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.ShareResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.ShareResult
  ( ShareResult (..),

    -- * Smart constructor
    mkShareResult,

    -- * Lenses
    srInviteePrincipalId,
    srPrincipalId,
    srRole,
    srShareId,
    srStatus,
    srStatusMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkDocs.Types.InviteePrincipalId as Types
import qualified Network.AWS.WorkDocs.Types.PrincipalId as Types
import qualified Network.AWS.WorkDocs.Types.RoleType as Types
import qualified Network.AWS.WorkDocs.Types.ShareId as Types
import qualified Network.AWS.WorkDocs.Types.ShareStatusType as Types
import qualified Network.AWS.WorkDocs.Types.StatusMessage as Types

-- | Describes the share results of a resource.
--
-- /See:/ 'mkShareResult' smart constructor.
data ShareResult = ShareResult'
  { -- | The ID of the invited user.
    inviteePrincipalId :: Core.Maybe Types.InviteePrincipalId,
    -- | The ID of the principal.
    principalId :: Core.Maybe Types.PrincipalId,
    -- | The role.
    role' :: Core.Maybe Types.RoleType,
    -- | The ID of the resource that was shared.
    shareId :: Core.Maybe Types.ShareId,
    -- | The status.
    status :: Core.Maybe Types.ShareStatusType,
    -- | The status message.
    statusMessage :: Core.Maybe Types.StatusMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ShareResult' value with any optional fields omitted.
mkShareResult ::
  ShareResult
mkShareResult =
  ShareResult'
    { inviteePrincipalId = Core.Nothing,
      principalId = Core.Nothing,
      role' = Core.Nothing,
      shareId = Core.Nothing,
      status = Core.Nothing,
      statusMessage = Core.Nothing
    }

-- | The ID of the invited user.
--
-- /Note:/ Consider using 'inviteePrincipalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srInviteePrincipalId :: Lens.Lens' ShareResult (Core.Maybe Types.InviteePrincipalId)
srInviteePrincipalId = Lens.field @"inviteePrincipalId"
{-# DEPRECATED srInviteePrincipalId "Use generic-lens or generic-optics with 'inviteePrincipalId' instead." #-}

-- | The ID of the principal.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPrincipalId :: Lens.Lens' ShareResult (Core.Maybe Types.PrincipalId)
srPrincipalId = Lens.field @"principalId"
{-# DEPRECATED srPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRole :: Lens.Lens' ShareResult (Core.Maybe Types.RoleType)
srRole = Lens.field @"role'"
{-# DEPRECATED srRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The ID of the resource that was shared.
--
-- /Note:/ Consider using 'shareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srShareId :: Lens.Lens' ShareResult (Core.Maybe Types.ShareId)
srShareId = Lens.field @"shareId"
{-# DEPRECATED srShareId "Use generic-lens or generic-optics with 'shareId' instead." #-}

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatus :: Lens.Lens' ShareResult (Core.Maybe Types.ShareStatusType)
srStatus = Lens.field @"status"
{-# DEPRECATED srStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatusMessage :: Lens.Lens' ShareResult (Core.Maybe Types.StatusMessage)
srStatusMessage = Lens.field @"statusMessage"
{-# DEPRECATED srStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

instance Core.FromJSON ShareResult where
  parseJSON =
    Core.withObject "ShareResult" Core.$
      \x ->
        ShareResult'
          Core.<$> (x Core..:? "InviteePrincipalId")
          Core.<*> (x Core..:? "PrincipalId")
          Core.<*> (x Core..:? "Role")
          Core.<*> (x Core..:? "ShareId")
          Core.<*> (x Core..:? "Status")
          Core.<*> (x Core..:? "StatusMessage")
