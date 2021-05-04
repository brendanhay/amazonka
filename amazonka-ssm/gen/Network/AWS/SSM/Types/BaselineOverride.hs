{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.BaselineOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.BaselineOverride where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.OperatingSystem
import Network.AWS.SSM.Types.PatchAction
import Network.AWS.SSM.Types.PatchComplianceLevel
import Network.AWS.SSM.Types.PatchFilterGroup
import Network.AWS.SSM.Types.PatchRuleGroup
import Network.AWS.SSM.Types.PatchSource

-- | Defines the basic information about a patch baseline override.
--
-- /See:/ 'newBaselineOverride' smart constructor.
data BaselineOverride = BaselineOverride'
  { -- | Information about the patches to use to update the instances, including
    -- target operating systems and source repositories. Applies to Linux
    -- instances only.
    sources :: Prelude.Maybe [PatchSource],
    -- | A list of explicitly rejected patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /AWS Systems Manager User Guide/.
    rejectedPatches :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the list of approved patches includes non-security
    -- updates that should be applied to the instances. The default value is
    -- \'false\'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Prelude.Maybe Prelude.Bool,
    -- | Defines the compliance level for approved patches. When an approved
    -- patch is reported as missing, this value describes the severity of the
    -- compliance violation.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | A list of explicitly approved patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /AWS Systems Manager User Guide/.
    approvedPatches :: Prelude.Maybe [Prelude.Text],
    -- | The action for Patch Manager to take on patches included in the
    -- RejectedPackages list. A patch can be allowed only if it is a dependency
    -- of another package, or blocked entirely along with packages that include
    -- it as a dependency.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    -- | The operating system rule used by the patch baseline override.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    approvalRules :: Prelude.Maybe PatchRuleGroup
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'BaselineOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'baselineOverride_sources' - Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
--
-- 'rejectedPatches', 'baselineOverride_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
--
-- 'approvedPatchesEnableNonSecurity', 'baselineOverride_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
--
-- 'approvedPatchesComplianceLevel', 'baselineOverride_approvedPatchesComplianceLevel' - Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation.
--
-- 'approvedPatches', 'baselineOverride_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
--
-- 'rejectedPatchesAction', 'baselineOverride_rejectedPatchesAction' - The action for Patch Manager to take on patches included in the
-- RejectedPackages list. A patch can be allowed only if it is a dependency
-- of another package, or blocked entirely along with packages that include
-- it as a dependency.
--
-- 'operatingSystem', 'baselineOverride_operatingSystem' - The operating system rule used by the patch baseline override.
--
-- 'globalFilters', 'baselineOverride_globalFilters' - Undocumented member.
--
-- 'approvalRules', 'baselineOverride_approvalRules' - Undocumented member.
newBaselineOverride ::
  BaselineOverride
newBaselineOverride =
  BaselineOverride'
    { sources = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      approvalRules = Prelude.Nothing
    }

-- | Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
baselineOverride_sources :: Lens.Lens' BaselineOverride (Prelude.Maybe [PatchSource])
baselineOverride_sources = Lens.lens (\BaselineOverride' {sources} -> sources) (\s@BaselineOverride' {} a -> s {sources = a} :: BaselineOverride) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
baselineOverride_rejectedPatches :: Lens.Lens' BaselineOverride (Prelude.Maybe [Prelude.Text])
baselineOverride_rejectedPatches = Lens.lens (\BaselineOverride' {rejectedPatches} -> rejectedPatches) (\s@BaselineOverride' {} a -> s {rejectedPatches = a} :: BaselineOverride) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
baselineOverride_approvedPatchesEnableNonSecurity :: Lens.Lens' BaselineOverride (Prelude.Maybe Prelude.Bool)
baselineOverride_approvedPatchesEnableNonSecurity = Lens.lens (\BaselineOverride' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@BaselineOverride' {} a -> s {approvedPatchesEnableNonSecurity = a} :: BaselineOverride)

-- | Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation.
baselineOverride_approvedPatchesComplianceLevel :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchComplianceLevel)
baselineOverride_approvedPatchesComplianceLevel = Lens.lens (\BaselineOverride' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@BaselineOverride' {} a -> s {approvedPatchesComplianceLevel = a} :: BaselineOverride)

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
baselineOverride_approvedPatches :: Lens.Lens' BaselineOverride (Prelude.Maybe [Prelude.Text])
baselineOverride_approvedPatches = Lens.lens (\BaselineOverride' {approvedPatches} -> approvedPatches) (\s@BaselineOverride' {} a -> s {approvedPatches = a} :: BaselineOverride) Prelude.. Lens.mapping Prelude._Coerce

-- | The action for Patch Manager to take on patches included in the
-- RejectedPackages list. A patch can be allowed only if it is a dependency
-- of another package, or blocked entirely along with packages that include
-- it as a dependency.
baselineOverride_rejectedPatchesAction :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchAction)
baselineOverride_rejectedPatchesAction = Lens.lens (\BaselineOverride' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@BaselineOverride' {} a -> s {rejectedPatchesAction = a} :: BaselineOverride)

-- | The operating system rule used by the patch baseline override.
baselineOverride_operatingSystem :: Lens.Lens' BaselineOverride (Prelude.Maybe OperatingSystem)
baselineOverride_operatingSystem = Lens.lens (\BaselineOverride' {operatingSystem} -> operatingSystem) (\s@BaselineOverride' {} a -> s {operatingSystem = a} :: BaselineOverride)

-- | Undocumented member.
baselineOverride_globalFilters :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchFilterGroup)
baselineOverride_globalFilters = Lens.lens (\BaselineOverride' {globalFilters} -> globalFilters) (\s@BaselineOverride' {} a -> s {globalFilters = a} :: BaselineOverride)

-- | Undocumented member.
baselineOverride_approvalRules :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchRuleGroup)
baselineOverride_approvalRules = Lens.lens (\BaselineOverride' {approvalRules} -> approvalRules) (\s@BaselineOverride' {} a -> s {approvalRules = a} :: BaselineOverride)

instance Prelude.Hashable BaselineOverride

instance Prelude.NFData BaselineOverride

instance Prelude.ToJSON BaselineOverride where
  toJSON BaselineOverride' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Sources" Prelude..=) Prelude.<$> sources,
            ("RejectedPatches" Prelude..=)
              Prelude.<$> rejectedPatches,
            ("ApprovedPatchesEnableNonSecurity" Prelude..=)
              Prelude.<$> approvedPatchesEnableNonSecurity,
            ("ApprovedPatchesComplianceLevel" Prelude..=)
              Prelude.<$> approvedPatchesComplianceLevel,
            ("ApprovedPatches" Prelude..=)
              Prelude.<$> approvedPatches,
            ("RejectedPatchesAction" Prelude..=)
              Prelude.<$> rejectedPatchesAction,
            ("OperatingSystem" Prelude..=)
              Prelude.<$> operatingSystem,
            ("GlobalFilters" Prelude..=)
              Prelude.<$> globalFilters,
            ("ApprovalRules" Prelude..=)
              Prelude.<$> approvalRules
          ]
      )
