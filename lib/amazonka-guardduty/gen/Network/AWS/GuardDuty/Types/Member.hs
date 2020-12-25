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
    mAccountId,
    mMasterId,
    mEmail,
    mRelationshipStatus,
    mUpdatedAt,
    mDetectorId,
    mInvitedAt,
  )
where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.DetectorId as Types
import qualified Network.AWS.GuardDuty.Types.Email as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the member account.
--
-- /See:/ 'mkMember' smart constructor.
data Member = Member'
  { -- | The ID of the member account.
    accountId :: Types.AccountId,
    -- | The master account ID.
    masterId :: Types.String,
    -- | The email address of the member account.
    email :: Types.Email,
    -- | The status of the relationship between the member and the master.
    relationshipStatus :: Types.String,
    -- | The last-updated timestamp of the member.
    updatedAt :: Types.String,
    -- | The detector ID of the member account.
    detectorId :: Core.Maybe Types.DetectorId,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Member' value with any optional fields omitted.
mkMember ::
  -- | 'accountId'
  Types.AccountId ->
  -- | 'masterId'
  Types.String ->
  -- | 'email'
  Types.Email ->
  -- | 'relationshipStatus'
  Types.String ->
  -- | 'updatedAt'
  Types.String ->
  Member
mkMember accountId masterId email relationshipStatus updatedAt =
  Member'
    { accountId,
      masterId,
      email,
      relationshipStatus,
      updatedAt,
      detectorId = Core.Nothing,
      invitedAt = Core.Nothing
    }

-- | The ID of the member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAccountId :: Lens.Lens' Member Types.AccountId
mAccountId = Lens.field @"accountId"
{-# DEPRECATED mAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The master account ID.
--
-- /Note:/ Consider using 'masterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMasterId :: Lens.Lens' Member Types.String
mMasterId = Lens.field @"masterId"
{-# DEPRECATED mMasterId "Use generic-lens or generic-optics with 'masterId' instead." #-}

-- | The email address of the member account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEmail :: Lens.Lens' Member Types.Email
mEmail = Lens.field @"email"
{-# DEPRECATED mEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The status of the relationship between the member and the master.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRelationshipStatus :: Lens.Lens' Member Types.String
mRelationshipStatus = Lens.field @"relationshipStatus"
{-# DEPRECATED mRelationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead." #-}

-- | The last-updated timestamp of the member.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mUpdatedAt :: Lens.Lens' Member Types.String
mUpdatedAt = Lens.field @"updatedAt"
{-# DEPRECATED mUpdatedAt "Use generic-lens or generic-optics with 'updatedAt' instead." #-}

-- | The detector ID of the member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDetectorId :: Lens.Lens' Member (Core.Maybe Types.DetectorId)
mDetectorId = Lens.field @"detectorId"
{-# DEPRECATED mDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInvitedAt :: Lens.Lens' Member (Core.Maybe Types.String)
mInvitedAt = Lens.field @"invitedAt"
{-# DEPRECATED mInvitedAt "Use generic-lens or generic-optics with 'invitedAt' instead." #-}

instance Core.FromJSON Member where
  parseJSON =
    Core.withObject "Member" Core.$
      \x ->
        Member'
          Core.<$> (x Core..: "accountId")
          Core.<*> (x Core..: "masterId")
          Core.<*> (x Core..: "email")
          Core.<*> (x Core..: "relationshipStatus")
          Core.<*> (x Core..: "updatedAt")
          Core.<*> (x Core..:? "detectorId")
          Core.<*> (x Core..:? "invitedAt")
