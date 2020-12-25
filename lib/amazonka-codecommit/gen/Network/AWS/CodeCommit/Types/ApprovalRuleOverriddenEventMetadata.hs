{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
  ( ApprovalRuleOverriddenEventMetadata (..),

    -- * Smart constructor
    mkApprovalRuleOverriddenEventMetadata,

    -- * Lenses
    aroemOverrideStatus,
    aroemRevisionId,
  )
where

import qualified Network.AWS.CodeCommit.Types.OverrideStatus as Types
import qualified Network.AWS.CodeCommit.Types.RevisionId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about an override event for approval rules for a pull request.
--
-- /See:/ 'mkApprovalRuleOverriddenEventMetadata' smart constructor.
data ApprovalRuleOverriddenEventMetadata = ApprovalRuleOverriddenEventMetadata'
  { -- | The status of the override event.
    overrideStatus :: Core.Maybe Types.OverrideStatus,
    -- | The revision ID of the pull request when the override event occurred.
    revisionId :: Core.Maybe Types.RevisionId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApprovalRuleOverriddenEventMetadata' value with any optional fields omitted.
mkApprovalRuleOverriddenEventMetadata ::
  ApprovalRuleOverriddenEventMetadata
mkApprovalRuleOverriddenEventMetadata =
  ApprovalRuleOverriddenEventMetadata'
    { overrideStatus =
        Core.Nothing,
      revisionId = Core.Nothing
    }

-- | The status of the override event.
--
-- /Note:/ Consider using 'overrideStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aroemOverrideStatus :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Core.Maybe Types.OverrideStatus)
aroemOverrideStatus = Lens.field @"overrideStatus"
{-# DEPRECATED aroemOverrideStatus "Use generic-lens or generic-optics with 'overrideStatus' instead." #-}

-- | The revision ID of the pull request when the override event occurred.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aroemRevisionId :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Core.Maybe Types.RevisionId)
aroemRevisionId = Lens.field @"revisionId"
{-# DEPRECATED aroemRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Core.FromJSON ApprovalRuleOverriddenEventMetadata where
  parseJSON =
    Core.withObject "ApprovalRuleOverriddenEventMetadata" Core.$
      \x ->
        ApprovalRuleOverriddenEventMetadata'
          Core.<$> (x Core..:? "overrideStatus") Core.<*> (x Core..:? "revisionId")
