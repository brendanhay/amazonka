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
-- Module      : Amazonka.SSM.Types.BaselineOverride
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.BaselineOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.OperatingSystem
import Amazonka.SSM.Types.PatchAction
import Amazonka.SSM.Types.PatchComplianceLevel
import Amazonka.SSM.Types.PatchFilterGroup
import Amazonka.SSM.Types.PatchRuleGroup
import Amazonka.SSM.Types.PatchSource

-- | Defines the basic information about a patch baseline override.
--
-- /See:/ 'newBaselineOverride' smart constructor.
data BaselineOverride = BaselineOverride'
  { -- | The operating system rule used by the patch baseline override.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A list of explicitly approved patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    approvedPatches :: Prelude.Maybe [Prelude.Text],
    -- | Defines the compliance level for approved patches. When an approved
    -- patch is reported as missing, this value describes the severity of the
    -- compliance violation.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | Information about the patches to use to update the managed nodes,
    -- including target operating systems and source repositories. Applies to
    -- Linux managed nodes only.
    sources :: Prelude.Maybe [PatchSource],
    approvalRules :: Prelude.Maybe PatchRuleGroup,
    -- | The action for Patch Manager to take on patches included in the
    -- @RejectedPackages@ list. A patch can be allowed only if it is a
    -- dependency of another package, or blocked entirely along with packages
    -- that include it as a dependency.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    -- | A list of explicitly rejected patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    rejectedPatches :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the list of approved patches includes non-security
    -- updates that should be applied to the managed nodes. The default value
    -- is @false@. Applies to Linux managed nodes only.
    approvedPatchesEnableNonSecurity :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BaselineOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystem', 'baselineOverride_operatingSystem' - The operating system rule used by the patch baseline override.
--
-- 'approvedPatches', 'baselineOverride_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'approvedPatchesComplianceLevel', 'baselineOverride_approvedPatchesComplianceLevel' - Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation.
--
-- 'sources', 'baselineOverride_sources' - Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
--
-- 'approvalRules', 'baselineOverride_approvalRules' - Undocumented member.
--
-- 'rejectedPatchesAction', 'baselineOverride_rejectedPatchesAction' - The action for Patch Manager to take on patches included in the
-- @RejectedPackages@ list. A patch can be allowed only if it is a
-- dependency of another package, or blocked entirely along with packages
-- that include it as a dependency.
--
-- 'globalFilters', 'baselineOverride_globalFilters' - Undocumented member.
--
-- 'rejectedPatches', 'baselineOverride_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'approvedPatchesEnableNonSecurity', 'baselineOverride_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
newBaselineOverride ::
  BaselineOverride
newBaselineOverride =
  BaselineOverride'
    { operatingSystem =
        Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      sources = Prelude.Nothing,
      approvalRules = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity = Prelude.Nothing
    }

-- | The operating system rule used by the patch baseline override.
baselineOverride_operatingSystem :: Lens.Lens' BaselineOverride (Prelude.Maybe OperatingSystem)
baselineOverride_operatingSystem = Lens.lens (\BaselineOverride' {operatingSystem} -> operatingSystem) (\s@BaselineOverride' {} a -> s {operatingSystem = a} :: BaselineOverride)

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
baselineOverride_approvedPatches :: Lens.Lens' BaselineOverride (Prelude.Maybe [Prelude.Text])
baselineOverride_approvedPatches = Lens.lens (\BaselineOverride' {approvedPatches} -> approvedPatches) (\s@BaselineOverride' {} a -> s {approvedPatches = a} :: BaselineOverride) Prelude.. Lens.mapping Lens.coerced

-- | Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation.
baselineOverride_approvedPatchesComplianceLevel :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchComplianceLevel)
baselineOverride_approvedPatchesComplianceLevel = Lens.lens (\BaselineOverride' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@BaselineOverride' {} a -> s {approvedPatchesComplianceLevel = a} :: BaselineOverride)

-- | Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
baselineOverride_sources :: Lens.Lens' BaselineOverride (Prelude.Maybe [PatchSource])
baselineOverride_sources = Lens.lens (\BaselineOverride' {sources} -> sources) (\s@BaselineOverride' {} a -> s {sources = a} :: BaselineOverride) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
baselineOverride_approvalRules :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchRuleGroup)
baselineOverride_approvalRules = Lens.lens (\BaselineOverride' {approvalRules} -> approvalRules) (\s@BaselineOverride' {} a -> s {approvalRules = a} :: BaselineOverride)

-- | The action for Patch Manager to take on patches included in the
-- @RejectedPackages@ list. A patch can be allowed only if it is a
-- dependency of another package, or blocked entirely along with packages
-- that include it as a dependency.
baselineOverride_rejectedPatchesAction :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchAction)
baselineOverride_rejectedPatchesAction = Lens.lens (\BaselineOverride' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@BaselineOverride' {} a -> s {rejectedPatchesAction = a} :: BaselineOverride)

-- | Undocumented member.
baselineOverride_globalFilters :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchFilterGroup)
baselineOverride_globalFilters = Lens.lens (\BaselineOverride' {globalFilters} -> globalFilters) (\s@BaselineOverride' {} a -> s {globalFilters = a} :: BaselineOverride)

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
baselineOverride_rejectedPatches :: Lens.Lens' BaselineOverride (Prelude.Maybe [Prelude.Text])
baselineOverride_rejectedPatches = Lens.lens (\BaselineOverride' {rejectedPatches} -> rejectedPatches) (\s@BaselineOverride' {} a -> s {rejectedPatches = a} :: BaselineOverride) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
baselineOverride_approvedPatchesEnableNonSecurity :: Lens.Lens' BaselineOverride (Prelude.Maybe Prelude.Bool)
baselineOverride_approvedPatchesEnableNonSecurity = Lens.lens (\BaselineOverride' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@BaselineOverride' {} a -> s {approvedPatchesEnableNonSecurity = a} :: BaselineOverride)

instance Prelude.Hashable BaselineOverride where
  hashWithSalt _salt BaselineOverride' {..} =
    _salt `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` approvedPatches
      `Prelude.hashWithSalt` approvedPatchesComplianceLevel
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` approvalRules
      `Prelude.hashWithSalt` rejectedPatchesAction
      `Prelude.hashWithSalt` globalFilters
      `Prelude.hashWithSalt` rejectedPatches
      `Prelude.hashWithSalt` approvedPatchesEnableNonSecurity

instance Prelude.NFData BaselineOverride where
  rnf BaselineOverride' {..} =
    Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf approvedPatches
      `Prelude.seq` Prelude.rnf approvedPatchesComplianceLevel
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf approvalRules
      `Prelude.seq` Prelude.rnf rejectedPatchesAction
      `Prelude.seq` Prelude.rnf globalFilters
      `Prelude.seq` Prelude.rnf rejectedPatches
      `Prelude.seq` Prelude.rnf approvedPatchesEnableNonSecurity

instance Core.ToJSON BaselineOverride where
  toJSON BaselineOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OperatingSystem" Core..=)
              Prelude.<$> operatingSystem,
            ("ApprovedPatches" Core..=)
              Prelude.<$> approvedPatches,
            ("ApprovedPatchesComplianceLevel" Core..=)
              Prelude.<$> approvedPatchesComplianceLevel,
            ("Sources" Core..=) Prelude.<$> sources,
            ("ApprovalRules" Core..=) Prelude.<$> approvalRules,
            ("RejectedPatchesAction" Core..=)
              Prelude.<$> rejectedPatchesAction,
            ("GlobalFilters" Core..=) Prelude.<$> globalFilters,
            ("RejectedPatches" Core..=)
              Prelude.<$> rejectedPatches,
            ("ApprovedPatchesEnableNonSecurity" Core..=)
              Prelude.<$> approvedPatchesEnableNonSecurity
          ]
      )
