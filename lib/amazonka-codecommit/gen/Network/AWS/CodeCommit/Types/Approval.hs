{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Approval
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Approval
  ( Approval (..),

    -- * Smart constructor
    mkApproval,

    -- * Lenses
    aApprovalState,
    aUserArn,
  )
where

import qualified Network.AWS.CodeCommit.Types.ApprovalState as Types
import qualified Network.AWS.CodeCommit.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a specific approval on a pull request.
--
-- /See:/ 'mkApproval' smart constructor.
data Approval = Approval'
  { -- | The state of the approval, APPROVE or REVOKE. REVOKE states are not stored.
    approvalState :: Core.Maybe Types.ApprovalState,
    -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Core.Maybe Types.Arn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Approval' value with any optional fields omitted.
mkApproval ::
  Approval
mkApproval =
  Approval' {approvalState = Core.Nothing, userArn = Core.Nothing}

-- | The state of the approval, APPROVE or REVOKE. REVOKE states are not stored.
--
-- /Note:/ Consider using 'approvalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApprovalState :: Lens.Lens' Approval (Core.Maybe Types.ApprovalState)
aApprovalState = Lens.field @"approvalState"
{-# DEPRECATED aApprovalState "Use generic-lens or generic-optics with 'approvalState' instead." #-}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUserArn :: Lens.Lens' Approval (Core.Maybe Types.Arn)
aUserArn = Lens.field @"userArn"
{-# DEPRECATED aUserArn "Use generic-lens or generic-optics with 'userArn' instead." #-}

instance Core.FromJSON Approval where
  parseJSON =
    Core.withObject "Approval" Core.$
      \x ->
        Approval'
          Core.<$> (x Core..:? "approvalState") Core.<*> (x Core..:? "userArn")
