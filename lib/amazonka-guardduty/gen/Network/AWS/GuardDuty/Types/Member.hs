{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Member
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.Member
  ( Member (..),

    -- * Smart constructor
    mkMember,

    -- * Lenses
    mEmail,
    mInvitedAt,
    mRelationshipStatus,
    mMasterId,
    mAccountId,
    mDetectorId,
    mUpdatedAt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the member account.
--
-- /See:/ 'mkMember' smart constructor.
data Member = Member'
  { -- | The email address of the member account.
    email :: Lude.Text,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Lude.Maybe Lude.Text,
    -- | The status of the relationship between the member and the master.
    relationshipStatus :: Lude.Text,
    -- | The master account ID.
    masterId :: Lude.Text,
    -- | The ID of the member account.
    accountId :: Lude.Text,
    -- | The detector ID of the member account.
    detectorId :: Lude.Maybe Lude.Text,
    -- | The last-updated timestamp of the member.
    updatedAt :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Member' with the minimum fields required to make a request.
--
-- * 'email' - The email address of the member account.
-- * 'invitedAt' - The timestamp when the invitation was sent.
-- * 'relationshipStatus' - The status of the relationship between the member and the master.
-- * 'masterId' - The master account ID.
-- * 'accountId' - The ID of the member account.
-- * 'detectorId' - The detector ID of the member account.
-- * 'updatedAt' - The last-updated timestamp of the member.
mkMember ::
  -- | 'email'
  Lude.Text ->
  -- | 'relationshipStatus'
  Lude.Text ->
  -- | 'masterId'
  Lude.Text ->
  -- | 'accountId'
  Lude.Text ->
  -- | 'updatedAt'
  Lude.Text ->
  Member
mkMember
  pEmail_
  pRelationshipStatus_
  pMasterId_
  pAccountId_
  pUpdatedAt_ =
    Member'
      { email = pEmail_,
        invitedAt = Lude.Nothing,
        relationshipStatus = pRelationshipStatus_,
        masterId = pMasterId_,
        accountId = pAccountId_,
        detectorId = Lude.Nothing,
        updatedAt = pUpdatedAt_
      }

-- | The email address of the member account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEmail :: Lens.Lens' Member Lude.Text
mEmail = Lens.lens (email :: Member -> Lude.Text) (\s a -> s {email = a} :: Member)
{-# DEPRECATED mEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInvitedAt :: Lens.Lens' Member (Lude.Maybe Lude.Text)
mInvitedAt = Lens.lens (invitedAt :: Member -> Lude.Maybe Lude.Text) (\s a -> s {invitedAt = a} :: Member)
{-# DEPRECATED mInvitedAt "Use generic-lens or generic-optics with 'invitedAt' instead." #-}

-- | The status of the relationship between the member and the master.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRelationshipStatus :: Lens.Lens' Member Lude.Text
mRelationshipStatus = Lens.lens (relationshipStatus :: Member -> Lude.Text) (\s a -> s {relationshipStatus = a} :: Member)
{-# DEPRECATED mRelationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead." #-}

-- | The master account ID.
--
-- /Note:/ Consider using 'masterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMasterId :: Lens.Lens' Member Lude.Text
mMasterId = Lens.lens (masterId :: Member -> Lude.Text) (\s a -> s {masterId = a} :: Member)
{-# DEPRECATED mMasterId "Use generic-lens or generic-optics with 'masterId' instead." #-}

-- | The ID of the member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAccountId :: Lens.Lens' Member Lude.Text
mAccountId = Lens.lens (accountId :: Member -> Lude.Text) (\s a -> s {accountId = a} :: Member)
{-# DEPRECATED mAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The detector ID of the member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDetectorId :: Lens.Lens' Member (Lude.Maybe Lude.Text)
mDetectorId = Lens.lens (detectorId :: Member -> Lude.Maybe Lude.Text) (\s a -> s {detectorId = a} :: Member)
{-# DEPRECATED mDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The last-updated timestamp of the member.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mUpdatedAt :: Lens.Lens' Member Lude.Text
mUpdatedAt = Lens.lens (updatedAt :: Member -> Lude.Text) (\s a -> s {updatedAt = a} :: Member)
{-# DEPRECATED mUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

instance Lude.FromJSON Member where
  parseJSON =
    Lude.withObject
      "Member"
      ( \x ->
          Member'
            Lude.<$> (x Lude..: "email")
            Lude.<*> (x Lude..:? "invitedAt")
            Lude.<*> (x Lude..: "relationshipStatus")
            Lude.<*> (x Lude..: "masterId")
            Lude.<*> (x Lude..: "accountId")
            Lude.<*> (x Lude..:? "detectorId")
            Lude.<*> (x Lude..: "updatedAt")
      )
