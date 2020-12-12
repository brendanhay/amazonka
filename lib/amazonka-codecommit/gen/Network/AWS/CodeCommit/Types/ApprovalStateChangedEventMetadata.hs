{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
  ( ApprovalStateChangedEventMetadata (..),

    -- * Smart constructor
    mkApprovalStateChangedEventMetadata,

    -- * Lenses
    ascemApprovalStatus,
    ascemRevisionId,
  )
where

import Network.AWS.CodeCommit.Types.ApprovalState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about a change in the approval state for a pull request.
--
-- /See:/ 'mkApprovalStateChangedEventMetadata' smart constructor.
data ApprovalStateChangedEventMetadata = ApprovalStateChangedEventMetadata'
  { approvalStatus ::
      Lude.Maybe
        ApprovalState,
    revisionId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApprovalStateChangedEventMetadata' with the minimum fields required to make a request.
--
-- * 'approvalStatus' - The approval status for the pull request.
-- * 'revisionId' - The revision ID of the pull request when the approval state changed.
mkApprovalStateChangedEventMetadata ::
  ApprovalStateChangedEventMetadata
mkApprovalStateChangedEventMetadata =
  ApprovalStateChangedEventMetadata'
    { approvalStatus = Lude.Nothing,
      revisionId = Lude.Nothing
    }

-- | The approval status for the pull request.
--
-- /Note:/ Consider using 'approvalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascemApprovalStatus :: Lens.Lens' ApprovalStateChangedEventMetadata (Lude.Maybe ApprovalState)
ascemApprovalStatus = Lens.lens (approvalStatus :: ApprovalStateChangedEventMetadata -> Lude.Maybe ApprovalState) (\s a -> s {approvalStatus = a} :: ApprovalStateChangedEventMetadata)
{-# DEPRECATED ascemApprovalStatus "Use generic-lens or generic-optics with 'approvalStatus' instead." #-}

-- | The revision ID of the pull request when the approval state changed.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascemRevisionId :: Lens.Lens' ApprovalStateChangedEventMetadata (Lude.Maybe Lude.Text)
ascemRevisionId = Lens.lens (revisionId :: ApprovalStateChangedEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: ApprovalStateChangedEventMetadata)
{-# DEPRECATED ascemRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.FromJSON ApprovalStateChangedEventMetadata where
  parseJSON =
    Lude.withObject
      "ApprovalStateChangedEventMetadata"
      ( \x ->
          ApprovalStateChangedEventMetadata'
            Lude.<$> (x Lude..:? "approvalStatus") Lude.<*> (x Lude..:? "revisionId")
      )
