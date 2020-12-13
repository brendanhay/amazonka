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

import Network.AWS.CodeCommit.Types.OverrideStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about an override event for approval rules for a pull request.
--
-- /See:/ 'mkApprovalRuleOverriddenEventMetadata' smart constructor.
data ApprovalRuleOverriddenEventMetadata = ApprovalRuleOverriddenEventMetadata'
  { -- | The status of the override event.
    overrideStatus :: Lude.Maybe OverrideStatus,
    -- | The revision ID of the pull request when the override event occurred.
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApprovalRuleOverriddenEventMetadata' with the minimum fields required to make a request.
--
-- * 'overrideStatus' - The status of the override event.
-- * 'revisionId' - The revision ID of the pull request when the override event occurred.
mkApprovalRuleOverriddenEventMetadata ::
  ApprovalRuleOverriddenEventMetadata
mkApprovalRuleOverriddenEventMetadata =
  ApprovalRuleOverriddenEventMetadata'
    { overrideStatus =
        Lude.Nothing,
      revisionId = Lude.Nothing
    }

-- | The status of the override event.
--
-- /Note:/ Consider using 'overrideStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aroemOverrideStatus :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Lude.Maybe OverrideStatus)
aroemOverrideStatus = Lens.lens (overrideStatus :: ApprovalRuleOverriddenEventMetadata -> Lude.Maybe OverrideStatus) (\s a -> s {overrideStatus = a} :: ApprovalRuleOverriddenEventMetadata)
{-# DEPRECATED aroemOverrideStatus "Use generic-lens or generic-optics with 'overrideStatus' instead." #-}

-- | The revision ID of the pull request when the override event occurred.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aroemRevisionId :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Lude.Maybe Lude.Text)
aroemRevisionId = Lens.lens (revisionId :: ApprovalRuleOverriddenEventMetadata -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: ApprovalRuleOverriddenEventMetadata)
{-# DEPRECATED aroemRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.FromJSON ApprovalRuleOverriddenEventMetadata where
  parseJSON =
    Lude.withObject
      "ApprovalRuleOverriddenEventMetadata"
      ( \x ->
          ApprovalRuleOverriddenEventMetadata'
            Lude.<$> (x Lude..:? "overrideStatus") Lude.<*> (x Lude..:? "revisionId")
      )
