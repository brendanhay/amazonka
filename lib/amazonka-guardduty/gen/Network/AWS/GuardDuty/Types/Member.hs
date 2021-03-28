{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Member
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Member
  ( Member (..)
  -- * Smart constructor
  , mkMember
  -- * Lenses
  , mAccountId
  , mMasterId
  , mEmail
  , mRelationshipStatus
  , mUpdatedAt
  , mDetectorId
  , mInvitedAt
  ) where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.DetectorId as Types
import qualified Network.AWS.GuardDuty.Types.Email as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the member account. 
--
-- /See:/ 'mkMember' smart constructor.
data Member = Member'
  { accountId :: Types.AccountId
    -- ^ The ID of the member account.
  , masterId :: Core.Text
    -- ^ The master account ID.
  , email :: Types.Email
    -- ^ The email address of the member account.
  , relationshipStatus :: Core.Text
    -- ^ The status of the relationship between the member and the master.
  , updatedAt :: Core.Text
    -- ^ The last-updated timestamp of the member.
  , detectorId :: Core.Maybe Types.DetectorId
    -- ^ The detector ID of the member account.
  , invitedAt :: Core.Maybe Core.Text
    -- ^ The timestamp when the invitation was sent.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Member' value with any optional fields omitted.
mkMember
    :: Types.AccountId -- ^ 'accountId'
    -> Core.Text -- ^ 'masterId'
    -> Types.Email -- ^ 'email'
    -> Core.Text -- ^ 'relationshipStatus'
    -> Core.Text -- ^ 'updatedAt'
    -> Member
mkMember accountId masterId email relationshipStatus updatedAt
  = Member'{accountId, masterId, email, relationshipStatus,
            updatedAt, detectorId = Core.Nothing, invitedAt = Core.Nothing}

-- | The ID of the member account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mAccountId :: Lens.Lens' Member Types.AccountId
mAccountId = Lens.field @"accountId"
{-# INLINEABLE mAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The master account ID.
--
-- /Note:/ Consider using 'masterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMasterId :: Lens.Lens' Member Core.Text
mMasterId = Lens.field @"masterId"
{-# INLINEABLE mMasterId #-}
{-# DEPRECATED masterId "Use generic-lens or generic-optics with 'masterId' instead"  #-}

-- | The email address of the member account.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mEmail :: Lens.Lens' Member Types.Email
mEmail = Lens.field @"email"
{-# INLINEABLE mEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The status of the relationship between the member and the master.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mRelationshipStatus :: Lens.Lens' Member Core.Text
mRelationshipStatus = Lens.field @"relationshipStatus"
{-# INLINEABLE mRelationshipStatus #-}
{-# DEPRECATED relationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead"  #-}

-- | The last-updated timestamp of the member.
--
-- /Note:/ Consider using 'updatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mUpdatedAt :: Lens.Lens' Member Core.Text
mUpdatedAt = Lens.field @"updatedAt"
{-# INLINEABLE mUpdatedAt #-}
{-# DEPRECATED updatedAt "Use generic-lens or generic-optics with 'updatedAt' instead"  #-}

-- | The detector ID of the member account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mDetectorId :: Lens.Lens' Member (Core.Maybe Types.DetectorId)
mDetectorId = Lens.field @"detectorId"
{-# INLINEABLE mDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mInvitedAt :: Lens.Lens' Member (Core.Maybe Core.Text)
mInvitedAt = Lens.field @"invitedAt"
{-# INLINEABLE mInvitedAt #-}
{-# DEPRECATED invitedAt "Use generic-lens or generic-optics with 'invitedAt' instead"  #-}

instance Core.FromJSON Member where
        parseJSON
          = Core.withObject "Member" Core.$
              \ x ->
                Member' Core.<$>
                  (x Core..: "accountId") Core.<*> x Core..: "masterId" Core.<*>
                    x Core..: "email"
                    Core.<*> x Core..: "relationshipStatus"
                    Core.<*> x Core..: "updatedAt"
                    Core.<*> x Core..:? "detectorId"
                    Core.<*> x Core..:? "invitedAt"
