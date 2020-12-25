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
    mfAccountId,
    mfInvitationId,
    mfInvitedAt,
    mfRelationshipStatus,
  )
where

import qualified Network.AWS.GuardDuty.Types.AccountId as Types
import qualified Network.AWS.GuardDuty.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the master account and invitation.
--
-- /See:/ 'mkMaster' smart constructor.
data Master = Master'
  { -- | The ID of the account used as the master account.
    accountId :: Core.Maybe Types.AccountId,
    -- | The value used to validate the master account to the member account.
    invitationId :: Core.Maybe Types.String,
    -- | The timestamp when the invitation was sent.
    invitedAt :: Core.Maybe Types.String,
    -- | The status of the relationship between the master and member accounts.
    relationshipStatus :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Master' value with any optional fields omitted.
mkMaster ::
  Master
mkMaster =
  Master'
    { accountId = Core.Nothing,
      invitationId = Core.Nothing,
      invitedAt = Core.Nothing,
      relationshipStatus = Core.Nothing
    }

-- | The ID of the account used as the master account.
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfAccountId :: Lens.Lens' Master (Core.Maybe Types.AccountId)
mfAccountId = Lens.field @"accountId"
{-# DEPRECATED mfAccountId "Use generic-lens or generic-optics with 'accountId' instead." #-}

-- | The value used to validate the master account to the member account.
--
-- /Note:/ Consider using 'invitationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfInvitationId :: Lens.Lens' Master (Core.Maybe Types.String)
mfInvitationId = Lens.field @"invitationId"
{-# DEPRECATED mfInvitationId "Use generic-lens or generic-optics with 'invitationId' instead." #-}

-- | The timestamp when the invitation was sent.
--
-- /Note:/ Consider using 'invitedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfInvitedAt :: Lens.Lens' Master (Core.Maybe Types.String)
mfInvitedAt = Lens.field @"invitedAt"
{-# DEPRECATED mfInvitedAt "Use generic-lens or generic-optics with 'invitedAt' instead." #-}

-- | The status of the relationship between the master and member accounts.
--
-- /Note:/ Consider using 'relationshipStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mfRelationshipStatus :: Lens.Lens' Master (Core.Maybe Types.String)
mfRelationshipStatus = Lens.field @"relationshipStatus"
{-# DEPRECATED mfRelationshipStatus "Use generic-lens or generic-optics with 'relationshipStatus' instead." #-}

instance Core.FromJSON Master where
  parseJSON =
    Core.withObject "Master" Core.$
      \x ->
        Master'
          Core.<$> (x Core..:? "accountId")
          Core.<*> (x Core..:? "invitationId")
          Core.<*> (x Core..:? "invitedAt")
          Core.<*> (x Core..:? "relationshipStatus")
