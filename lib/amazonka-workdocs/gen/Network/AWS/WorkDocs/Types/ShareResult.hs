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
    srStatus,
    srPrincipalId,
    srInviteePrincipalId,
    srRole,
    srStatusMessage,
    srShareId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WorkDocs.Types.RoleType
import Network.AWS.WorkDocs.Types.ShareStatusType

-- | Describes the share results of a resource.
--
-- /See:/ 'mkShareResult' smart constructor.
data ShareResult = ShareResult'
  { status ::
      Lude.Maybe ShareStatusType,
    principalId :: Lude.Maybe Lude.Text,
    inviteePrincipalId :: Lude.Maybe Lude.Text,
    role' :: Lude.Maybe RoleType,
    statusMessage :: Lude.Maybe (Lude.Sensitive Lude.Text),
    shareId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ShareResult' with the minimum fields required to make a request.
--
-- * 'inviteePrincipalId' - The ID of the invited user.
-- * 'principalId' - The ID of the principal.
-- * 'role'' - The role.
-- * 'shareId' - The ID of the resource that was shared.
-- * 'status' - The status.
-- * 'statusMessage' - The status message.
mkShareResult ::
  ShareResult
mkShareResult =
  ShareResult'
    { status = Lude.Nothing,
      principalId = Lude.Nothing,
      inviteePrincipalId = Lude.Nothing,
      role' = Lude.Nothing,
      statusMessage = Lude.Nothing,
      shareId = Lude.Nothing
    }

-- | The status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatus :: Lens.Lens' ShareResult (Lude.Maybe ShareStatusType)
srStatus = Lens.lens (status :: ShareResult -> Lude.Maybe ShareStatusType) (\s a -> s {status = a} :: ShareResult)
{-# DEPRECATED srStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the principal.
--
-- /Note:/ Consider using 'principalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srPrincipalId :: Lens.Lens' ShareResult (Lude.Maybe Lude.Text)
srPrincipalId = Lens.lens (principalId :: ShareResult -> Lude.Maybe Lude.Text) (\s a -> s {principalId = a} :: ShareResult)
{-# DEPRECATED srPrincipalId "Use generic-lens or generic-optics with 'principalId' instead." #-}

-- | The ID of the invited user.
--
-- /Note:/ Consider using 'inviteePrincipalId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srInviteePrincipalId :: Lens.Lens' ShareResult (Lude.Maybe Lude.Text)
srInviteePrincipalId = Lens.lens (inviteePrincipalId :: ShareResult -> Lude.Maybe Lude.Text) (\s a -> s {inviteePrincipalId = a} :: ShareResult)
{-# DEPRECATED srInviteePrincipalId "Use generic-lens or generic-optics with 'inviteePrincipalId' instead." #-}

-- | The role.
--
-- /Note:/ Consider using 'role'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srRole :: Lens.Lens' ShareResult (Lude.Maybe RoleType)
srRole = Lens.lens (role' :: ShareResult -> Lude.Maybe RoleType) (\s a -> s {role' = a} :: ShareResult)
{-# DEPRECATED srRole "Use generic-lens or generic-optics with 'role'' instead." #-}

-- | The status message.
--
-- /Note:/ Consider using 'statusMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatusMessage :: Lens.Lens' ShareResult (Lude.Maybe (Lude.Sensitive Lude.Text))
srStatusMessage = Lens.lens (statusMessage :: ShareResult -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {statusMessage = a} :: ShareResult)
{-# DEPRECATED srStatusMessage "Use generic-lens or generic-optics with 'statusMessage' instead." #-}

-- | The ID of the resource that was shared.
--
-- /Note:/ Consider using 'shareId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srShareId :: Lens.Lens' ShareResult (Lude.Maybe Lude.Text)
srShareId = Lens.lens (shareId :: ShareResult -> Lude.Maybe Lude.Text) (\s a -> s {shareId = a} :: ShareResult)
{-# DEPRECATED srShareId "Use generic-lens or generic-optics with 'shareId' instead." #-}

instance Lude.FromJSON ShareResult where
  parseJSON =
    Lude.withObject
      "ShareResult"
      ( \x ->
          ShareResult'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PrincipalId")
            Lude.<*> (x Lude..:? "InviteePrincipalId")
            Lude.<*> (x Lude..:? "Role")
            Lude.<*> (x Lude..:? "StatusMessage")
            Lude.<*> (x Lude..:? "ShareId")
      )
