{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Master
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Master
  ( Master (..)
  -- * Smart constructor
  , mkMaster
  -- * Lenses
  , mfAccountId
  , mfInvitationId
  , mfInvitedAt
  , mfRelationshipStatus
  ) where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the master account and invitation.
--
-- /See:/ 'mkMaster' smart constructor.
data Master = Master'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The ID of the account used as the master account.
  , invitationId :: Core.Maybe Core.Text
    -- ^ The value used to validate the master account to the member account.
  , invitedAt :: Core.Maybe Core.Text
    -- ^ The timestamp when the invitation was sent.
  , relationshipStatus :: Core.Maybe Core.Text
    -- ^ The status of the relationship between the master and member accounts.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Master' value with any optional fields omitted.
mkMaster
    :: Master
mkMaster
  = Master'{accountId = Core.Nothing, invitationId = Core.Nothing,
            invitedAt = Core.Nothing, relationshipStatus = Core.Nothing}

-- | The ID of the account used as the master account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfAccountId :: Lens.Lens' Master (Core.Maybe Types.AccountId)
mfAccountId = Lens.field @"accountId"
{-# INLINEABLE mfAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The value used to validate the master account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfInvitationId :: Lens.Lens' Master (Core.Maybe Core.Text)
mfInvitationId = Lens.field @"invitationId"
{-# INLINEABLE mfInvitationId #-}
{-# DEPRECATED invitationId "Use generic-lens or generic-optics with 'invitationId' instead"  #-}

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfInvitedAt :: Lens.Lens' Master (Core.Maybe Core.Text)
mfInvitedAt = Lens.field @"invitedAt"
{-# INLINEABLE mfInvitedAt #-}
{-# DEPRECATED invitedAt "Use generic-lens or generic-optics with 'invitedAt' instead"  #-}

-- | The status of the relationship between the master and member accounts.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfRelationshipStatus :: Lens.Lens' Master (Core.Maybe Core.Text)
mfRelationshipStatus = Lens.field @"relationshipStatus"
{-# INLINEABLE mfRelationshipStatus #-}
{-# DEPRECATED relationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead"  #-}

instance Core.FromJSON Master where
        parseJSON
          = Core.withObject "Master" Core.$
              \ x ->
                Master' Core.<$>
                  (x Core..:? "accountId") Core.<*> x Core..:? "invitationId"
                    Core.<*> x Core..:? "invitedAt"
                    Core.<*> x Core..:? "relationshipStatus"
