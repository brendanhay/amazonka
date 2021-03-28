{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ApprovalResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.ApprovalResult
  ( ApprovalResult (..)
  -- * Smart constructor
  , mkApprovalResult
  -- * Lenses
  , arSummary
  , arStatus
  ) where

import qualified Network.AWS.CodePipeline.Types.ApprovalStatus as Types
import qualified Network.AWS.CodePipeline.Types.ApprovalSummary as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the result of an approval request.
--
-- /See:/ 'mkApprovalResult' smart constructor.
data ApprovalResult = ApprovalResult'
  { summary :: Types.ApprovalSummary
    -- ^ The summary of the current status of the approval request.
  , status :: Types.ApprovalStatus
    -- ^ The response submitted by a reviewer assigned to an approval action request.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApprovalResult' value with any optional fields omitted.
mkApprovalResult
    :: Types.ApprovalSummary -- ^ 'summary'
    -> Types.ApprovalStatus -- ^ 'status'
    -> ApprovalResult
mkApprovalResult summary status = ApprovalResult'{summary, status}

-- | The summary of the current status of the approval request.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arSummary :: Lens.Lens' ApprovalResult Types.ApprovalSummary
arSummary = Lens.field @"summary"
{-# INLINEABLE arSummary #-}
{-# DEPRECATED summary "Use generic-lens or generic-optics with 'summary' instead"  #-}

-- | The response submitted by a reviewer assigned to an approval action request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
arStatus :: Lens.Lens' ApprovalResult Types.ApprovalStatus
arStatus = Lens.field @"status"
{-# INLINEABLE arStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON ApprovalResult where
        toJSON ApprovalResult{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("summary" Core..= summary),
                  Core.Just ("status" Core..= status)])
