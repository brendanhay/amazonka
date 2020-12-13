{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ApprovalResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ApprovalResult
  ( ApprovalResult (..),

    -- * Smart constructor
    mkApprovalResult,

    -- * Lenses
    arSummary,
    arStatus,
  )
where

import Network.AWS.CodePipeline.Types.ApprovalStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about the result of an approval request.
--
-- /See:/ 'mkApprovalResult' smart constructor.
data ApprovalResult = ApprovalResult'
  { -- | The summary of the current status of the approval request.
    summary :: Lude.Text,
    -- | The response submitted by a reviewer assigned to an approval action request.
    status :: ApprovalStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApprovalResult' with the minimum fields required to make a request.
--
-- * 'summary' - The summary of the current status of the approval request.
-- * 'status' - The response submitted by a reviewer assigned to an approval action request.
mkApprovalResult ::
  -- | 'summary'
  Lude.Text ->
  -- | 'status'
  ApprovalStatus ->
  ApprovalResult
mkApprovalResult pSummary_ pStatus_ =
  ApprovalResult' {summary = pSummary_, status = pStatus_}

-- | The summary of the current status of the approval request.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arSummary :: Lens.Lens' ApprovalResult Lude.Text
arSummary = Lens.lens (summary :: ApprovalResult -> Lude.Text) (\s a -> s {summary = a} :: ApprovalResult)
{-# DEPRECATED arSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The response submitted by a reviewer assigned to an approval action request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStatus :: Lens.Lens' ApprovalResult ApprovalStatus
arStatus = Lens.lens (status :: ApprovalResult -> ApprovalStatus) (\s a -> s {status = a} :: ApprovalResult)
{-# DEPRECATED arStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.ToJSON ApprovalResult where
  toJSON ApprovalResult' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("summary" Lude..= summary),
            Lude.Just ("status" Lude..= status)
          ]
      )
