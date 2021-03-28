{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeCommit.Types.ApprovalStateChangedEventMetadata
  ( ApprovalStateChangedEventMetadata (..)
  -- * Smart constructor
  , mkApprovalStateChangedEventMetadata
  -- * Lenses
  , ascemApprovalStatus
  , ascemRevisionId
  ) where

import qualified Network.AWS.CodeCommit.Types.ApprovalState as Types
import qualified Network.AWS.CodeCommit.Types.RevisionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about a change in the approval state for a pull request.
--
-- /See:/ 'mkApprovalStateChangedEventMetadata' smart constructor.
data ApprovalStateChangedEventMetadata = ApprovalStateChangedEventMetadata'
  { approvalStatus :: Core.Maybe Types.ApprovalState
    -- ^ The approval status for the pull request.
  , revisionId :: Core.Maybe Types.RevisionId
    -- ^ The revision ID of the pull request when the approval state changed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApprovalStateChangedEventMetadata' value with any optional fields omitted.
mkApprovalStateChangedEventMetadata
    :: ApprovalStateChangedEventMetadata
mkApprovalStateChangedEventMetadata
  = ApprovalStateChangedEventMetadata'{approvalStatus = Core.Nothing,
                                       revisionId = Core.Nothing}

-- | The approval status for the pull request.
--
-- /Note:/ Consider using 'approvalStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascemApprovalStatus :: Lens.Lens' ApprovalStateChangedEventMetadata (Core.Maybe Types.ApprovalState)
ascemApprovalStatus = Lens.field @"approvalStatus"
{-# INLINEABLE ascemApprovalStatus #-}
{-# DEPRECATED approvalStatus "Use generic-lens or generic-optics with 'approvalStatus' instead"  #-}

-- | The revision ID of the pull request when the approval state changed.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ascemRevisionId :: Lens.Lens' ApprovalStateChangedEventMetadata (Core.Maybe Types.RevisionId)
ascemRevisionId = Lens.field @"revisionId"
{-# INLINEABLE ascemRevisionId #-}
{-# DEPRECATED revisionId "Use generic-lens or generic-optics with 'revisionId' instead"  #-}

instance Core.FromJSON ApprovalStateChangedEventMetadata where
        parseJSON
          = Core.withObject "ApprovalStateChangedEventMetadata" Core.$
              \ x ->
                ApprovalStateChangedEventMetadata' Core.<$>
                  (x Core..:? "approvalStatus") Core.<*> x Core..:? "revisionId"
