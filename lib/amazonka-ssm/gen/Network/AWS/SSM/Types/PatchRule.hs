{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.PatchRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.PatchRule
  ( PatchRule (..),

    -- * Smart constructor
    mkPatchRule,

    -- * Lenses
    prApproveAfterDays,
    prApproveUntilDate,
    prPatchFilterGroup,
    prEnableNonSecurity,
    prComplianceLevel,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchFilterGroup

-- | Defines an approval rule for a patch baseline.
--
-- /See:/ 'mkPatchRule' smart constructor.
data PatchRule = PatchRule'
  { -- | The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
    approveAfterDays :: Lude.Maybe Lude.Natural,
    -- | The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server.
    --
    -- Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
    approveUntilDate :: Lude.Maybe Lude.Text,
    -- | The patch filter group that defines the criteria for the rule.
    patchFilterGroup :: PatchFilterGroup,
    -- | For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
    enableNonSecurity :: Lude.Maybe Lude.Bool,
    -- | A compliance severity level for all approved patches in a patch baseline.
    complianceLevel :: Lude.Maybe PatchComplianceLevel
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PatchRule' with the minimum fields required to make a request.
--
-- * 'approveAfterDays' - The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
-- * 'approveUntilDate' - The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server.
--
-- Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
-- * 'patchFilterGroup' - The patch filter group that defines the criteria for the rule.
-- * 'enableNonSecurity' - For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
-- * 'complianceLevel' - A compliance severity level for all approved patches in a patch baseline.
mkPatchRule ::
  -- | 'patchFilterGroup'
  PatchFilterGroup ->
  PatchRule
mkPatchRule pPatchFilterGroup_ =
  PatchRule'
    { approveAfterDays = Lude.Nothing,
      approveUntilDate = Lude.Nothing,
      patchFilterGroup = pPatchFilterGroup_,
      enableNonSecurity = Lude.Nothing,
      complianceLevel = Lude.Nothing
    }

-- | The number of days after the release date of each patch matched by the rule that the patch is marked as approved in the patch baseline. For example, a value of @7@ means that patches are approved seven days after they are released. Not supported on Ubuntu Server.
--
-- /Note:/ Consider using 'approveAfterDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prApproveAfterDays :: Lens.Lens' PatchRule (Lude.Maybe Lude.Natural)
prApproveAfterDays = Lens.lens (approveAfterDays :: PatchRule -> Lude.Maybe Lude.Natural) (\s a -> s {approveAfterDays = a} :: PatchRule)
{-# DEPRECATED prApproveAfterDays "Use generic-lens or generic-optics with 'approveAfterDays' instead." #-}

-- | The cutoff date for auto approval of released patches. Any patches released on or before this date are installed automatically. Not supported on Ubuntu Server.
--
-- Enter dates in the format @YYYY-MM-DD@ . For example, @2020-12-31@ .
--
-- /Note:/ Consider using 'approveUntilDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prApproveUntilDate :: Lens.Lens' PatchRule (Lude.Maybe Lude.Text)
prApproveUntilDate = Lens.lens (approveUntilDate :: PatchRule -> Lude.Maybe Lude.Text) (\s a -> s {approveUntilDate = a} :: PatchRule)
{-# DEPRECATED prApproveUntilDate "Use generic-lens or generic-optics with 'approveUntilDate' instead." #-}

-- | The patch filter group that defines the criteria for the rule.
--
-- /Note:/ Consider using 'patchFilterGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prPatchFilterGroup :: Lens.Lens' PatchRule PatchFilterGroup
prPatchFilterGroup = Lens.lens (patchFilterGroup :: PatchRule -> PatchFilterGroup) (\s a -> s {patchFilterGroup = a} :: PatchRule)
{-# DEPRECATED prPatchFilterGroup "Use generic-lens or generic-optics with 'patchFilterGroup' instead." #-}

-- | For instances identified by the approval rule filters, enables a patch baseline to apply non-security updates available in the specified repository. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'enableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prEnableNonSecurity :: Lens.Lens' PatchRule (Lude.Maybe Lude.Bool)
prEnableNonSecurity = Lens.lens (enableNonSecurity :: PatchRule -> Lude.Maybe Lude.Bool) (\s a -> s {enableNonSecurity = a} :: PatchRule)
{-# DEPRECATED prEnableNonSecurity "Use generic-lens or generic-optics with 'enableNonSecurity' instead." #-}

-- | A compliance severity level for all approved patches in a patch baseline.
--
-- /Note:/ Consider using 'complianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
prComplianceLevel :: Lens.Lens' PatchRule (Lude.Maybe PatchComplianceLevel)
prComplianceLevel = Lens.lens (complianceLevel :: PatchRule -> Lude.Maybe PatchComplianceLevel) (\s a -> s {complianceLevel = a} :: PatchRule)
{-# DEPRECATED prComplianceLevel "Use generic-lens or generic-optics with 'complianceLevel' instead." #-}

instance Lude.FromJSON PatchRule where
  parseJSON =
    Lude.withObject
      "PatchRule"
      ( \x ->
          PatchRule'
            Lude.<$> (x Lude..:? "ApproveAfterDays")
            Lude.<*> (x Lude..:? "ApproveUntilDate")
            Lude.<*> (x Lude..: "PatchFilterGroup")
            Lude.<*> (x Lude..:? "EnableNonSecurity")
            Lude.<*> (x Lude..:? "ComplianceLevel")
      )

instance Lude.ToJSON PatchRule where
  toJSON PatchRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ApproveAfterDays" Lude..=) Lude.<$> approveAfterDays,
            ("ApproveUntilDate" Lude..=) Lude.<$> approveUntilDate,
            Lude.Just ("PatchFilterGroup" Lude..= patchFilterGroup),
            ("EnableNonSecurity" Lude..=) Lude.<$> enableNonSecurity,
            ("ComplianceLevel" Lude..=) Lude.<$> complianceLevel
          ]
      )
