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
    aUserARN,
  )
where

import Network.AWS.CodeCommit.Types.ApprovalState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a specific approval on a pull request.
--
-- /See:/ 'mkApproval' smart constructor.
data Approval = Approval'
  { approvalState ::
      Lude.Maybe ApprovalState,
    userARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Approval' with the minimum fields required to make a request.
--
-- * 'approvalState' - The state of the approval, APPROVE or REVOKE. REVOKE states are not stored.
-- * 'userARN' - The Amazon Resource Name (ARN) of the user.
mkApproval ::
  Approval
mkApproval =
  Approval' {approvalState = Lude.Nothing, userARN = Lude.Nothing}

-- | The state of the approval, APPROVE or REVOKE. REVOKE states are not stored.
--
-- /Note:/ Consider using 'approvalState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aApprovalState :: Lens.Lens' Approval (Lude.Maybe ApprovalState)
aApprovalState = Lens.lens (approvalState :: Approval -> Lude.Maybe ApprovalState) (\s a -> s {approvalState = a} :: Approval)
{-# DEPRECATED aApprovalState "Use generic-lens or generic-optics with 'approvalState' instead." #-}

-- | The Amazon Resource Name (ARN) of the user.
--
-- /Note:/ Consider using 'userARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aUserARN :: Lens.Lens' Approval (Lude.Maybe Lude.Text)
aUserARN = Lens.lens (userARN :: Approval -> Lude.Maybe Lude.Text) (\s a -> s {userARN = a} :: Approval)
{-# DEPRECATED aUserARN "Use generic-lens or generic-optics with 'userARN' instead." #-}

instance Lude.FromJSON Approval where
  parseJSON =
    Lude.withObject
      "Approval"
      ( \x ->
          Approval'
            Lude.<$> (x Lude..:? "approvalState") Lude.<*> (x Lude..:? "userArn")
      )
