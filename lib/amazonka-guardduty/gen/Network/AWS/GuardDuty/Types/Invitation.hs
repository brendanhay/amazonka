{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.Invitation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.Invitation
  ( Invitation (..)
  -- * Smart constructor
  , mkInvitation
  -- * Lenses
  , iAccountId
  , iInvitationId
  , iInvitedAt
  , iRelationshipStatus
  ) where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the invitation to become a member account.
--
-- /See:/ 'mkInvitation' smart constructor.
data Invitation = Invitation'
  { accountId :: Core.Maybe Types.AccountId
    -- ^ The ID of the account that the invitation was sent from.
  , invitationId :: Core.Maybe Core.Text
    -- ^ The ID of the invitation. This value is used to validate the inviter account to the member account.
  , invitedAt :: Core.Maybe Core.Text
    -- ^ The timestamp when the invitation was sent.
  , relationshipStatus :: Core.Maybe Core.Text
    -- ^ The status of the relationship between the inviter and invitee accounts.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Invitation' value with any optional fields omitted.
mkInvitation
    :: Invitation
mkInvitation
  = Invitation'{accountId = Core.Nothing,
                invitationId = Core.Nothing, invitedAt = Core.Nothing,
                relationshipStatus = Core.Nothing}

-- | The ID of the account that the invitation was sent from.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iAccountId :: Lens.Lens' Invitation (Core.Maybe Types.AccountId)
iAccountId = Lens.field @"accountId"
{-# INLINEABLE iAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The ID of the invitation. This value is used to validate the inviter account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvitationId :: Lens.Lens' Invitation (Core.Maybe Core.Text)
iInvitationId = Lens.field @"invitationId"
{-# INLINEABLE iInvitationId #-}
{-# DEPRECATED invitationId "Use generic-lens or generic-optics with 'invitationId' instead"  #-}

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvitedAt :: Lens.Lens' Invitation (Core.Maybe Core.Text)
iInvitedAt = Lens.field @"invitedAt"
{-# INLINEABLE iInvitedAt #-}
{-# DEPRECATED invitedAt "Use generic-lens or generic-optics with 'invitedAt' instead"  #-}

-- | The status of the relationship between the inviter and invitee accounts.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iRelationshipStatus :: Lens.Lens' Invitation (Core.Maybe Core.Text)
iRelationshipStatus = Lens.field @"relationshipStatus"
{-# INLINEABLE iRelationshipStatus #-}
{-# DEPRECATED relationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead"  #-}

instance Core.FromJSON Invitation where
        parseJSON
          = Core.withObject "Invitation" Core.$
              \ x ->
                Invitation' Core.<$>
                  (x Core..:? "accountId") Core.<*> x Core..:? "invitationId"
                    Core.<*> x Core..:? "invitedAt"
                    Core.<*> x Core..:? "relationshipStatus"
