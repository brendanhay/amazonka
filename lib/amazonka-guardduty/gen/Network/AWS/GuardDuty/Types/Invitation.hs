-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Invitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Invitation
  ( Invitation (..),

    -- * Smart constructor
    mkInvitation,

    -- * Lenses
    iInvitedAt,
    iRelationshipStatus,
    iInvitationId,
    iAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the invitation to become a member account.
--
-- /See:/ 'mkInvitation' smart constructor.
data Invitation = Invitation'
  { invitedAt :: Lude.Maybe Lude.Text,
    relationshipStatus :: Lude.Maybe Lude.Text,
    invitationId :: Lude.Maybe Lude.Text,
    accountId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Invitation' with the minimum fields required to make a request.
--
-- * 'accountId' - The ID of the account that the invitation was sent from.
-- * 'invitationId' - The ID of the invitation. This value is used to validate the inviter account to the member account.
-- * 'invitedAt' - The timestamp when the invitation was sent.
-- * 'relationshipStatus' - The status of the relationship between the inviter and invitee accounts.
mkInvitation ::
  Invitation
mkInvitation =
  Invitation'
    { invitedAt = Lude.Nothing,
      relationshipStatus = Lude.Nothing,
      invitationId = Lude.Nothing,
      accountId = Lude.Nothing
    }

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvitedAt :: Lens.Lens' Invitation (Lude.Maybe Lude.Text)
iInvitedAt = Lens.lens (invitedAt :: Invitation -> Lude.Maybe Lude.Text) (\s a -> s {invitedAt = a} :: Invitation)
{-# DEPRECATED iInvitedAt "Use generic-lens or generic-optics with 'invitedAt' instead." #-}

-- | The status of the relationship between the inviter and invitee accounts.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRelationshipStatus :: Lens.Lens' Invitation (Lude.Maybe Lude.Text)
iRelationshipStatus = Lens.lens (relationshipStatus :: Invitation -> Lude.Maybe Lude.Text) (\s a -> s {relationshipStatus = a} :: Invitation)
{-# DEPRECATED iRelationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead." #-}

-- | The ID of the invitation. This value is used to validate the inviter account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvitationId :: Lens.Lens' Invitation (Lude.Maybe Lude.Text)
iInvitationId = Lens.lens (invitationId :: Invitation -> Lude.Maybe Lude.Text) (\s a -> s {invitationId = a} :: Invitation)
{-# DEPRECATED iInvitationId "Use generic-lens or generic-optics with 'invitationId' instead." #-}

-- | The ID of the account that the invitation was sent from.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAccountId :: Lens.Lens' Invitation (Lude.Maybe Lude.Text)
iAccountId = Lens.lens (accountId :: Invitation -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: Invitation)
{-# DEPRECATED iAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.FromJSON Invitation where
  parseJSON =
    Lude.withObject
      "Invitation"
      ( \x ->
          Invitation'
            Lude.<$> (x Lude..:? "invitedAt")
            Lude.<*> (x Lude..:? "relationshipStatus")
            Lude.<*> (x Lude..:? "invitationId")
            Lude.<*> (x Lude..:? "accountId")
      )
