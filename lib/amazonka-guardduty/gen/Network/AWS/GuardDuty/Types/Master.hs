{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Master
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Master
  ( Master (..),

    -- * Smart constructor
    mkMaster,

    -- * Lenses
    mfInvitedAt,
    mfRelationshipStatus,
    mfInvitationId,
    mfAccountId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the master account and invitation.
--
-- /See:/ 'mkMaster' smart constructor.
data Master = Master'
  { -- | The timestamp when the invitation was sent.
    invitedAt :: Lude.Maybe Lude.Text,
    -- | The status of the relationship between the master and member accounts.
    relationshipStatus :: Lude.Maybe Lude.Text,
    -- | The value used to validate the master account to the member account.
    invitationId :: Lude.Maybe Lude.Text,
    -- | The ID of the account used as the master account.
    accountId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Master' with the minimum fields required to make a request.
--
-- * 'invitedAt' - The timestamp when the invitation was sent.
-- * 'relationshipStatus' - The status of the relationship between the master and member accounts.
-- * 'invitationId' - The value used to validate the master account to the member account.
-- * 'accountId' - The ID of the account used as the master account.
mkMaster ::
  Master
mkMaster =
  Master'
    { invitedAt = Lude.Nothing,
      relationshipStatus = Lude.Nothing,
      invitationId = Lude.Nothing,
      accountId = Lude.Nothing
    }

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfInvitedAt :: Lens.Lens' Master (Lude.Maybe Lude.Text)
mfInvitedAt = Lens.lens (invitedAt :: Master -> Lude.Maybe Lude.Text) (\s a -> s {invitedAt = a} :: Master)
{-# DEPRECATED mfInvitedAt "Use generic-lens or generic-optics with 'invitedAt' instead." #-}

-- | The status of the relationship between the master and member accounts.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfRelationshipStatus :: Lens.Lens' Master (Lude.Maybe Lude.Text)
mfRelationshipStatus = Lens.lens (relationshipStatus :: Master -> Lude.Maybe Lude.Text) (\s a -> s {relationshipStatus = a} :: Master)
{-# DEPRECATED mfRelationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead." #-}

-- | The value used to validate the master account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfInvitationId :: Lens.Lens' Master (Lude.Maybe Lude.Text)
mfInvitationId = Lens.lens (invitationId :: Master -> Lude.Maybe Lude.Text) (\s a -> s {invitationId = a} :: Master)
{-# DEPRECATED mfInvitationId "Use generic-lens or generic-optics with 'invitationId' instead." #-}

-- | The ID of the account used as the master account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfAccountId :: Lens.Lens' Master (Lude.Maybe Lude.Text)
mfAccountId = Lens.lens (accountId :: Master -> Lude.Maybe Lude.Text) (\s a -> s {accountId = a} :: Master)
{-# DEPRECATED mfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

instance Lude.FromJSON Master where
  parseJSON =
    Lude.withObject
      "Master"
      ( \x ->
          Master'
            Lude.<$> (x Lude..:? "invitedAt")
            Lude.<*> (x Lude..:? "relationshipStatus")
            Lude.<*> (x Lude..:? "invitationId")
            Lude.<*> (x Lude..:? "accountId")
      )
