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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.BaselineOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
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
  { approvalRules :: Prelude.Maybe PatchRuleGroup,
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
    -- | Indicates whether the list of approved patches includes non-security
    -- updates that should be applied to the managed nodes. The default value
    -- is @false@. Applies to Linux managed nodes only.
    approvedPatchesEnableNonSecurity :: Prelude.Maybe Prelude.Bool,
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    -- | The operating system rule used by the patch baseline override.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A list of explicitly rejected patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    rejectedPatches :: Prelude.Maybe [Prelude.Text],
    -- | The action for Patch Manager to take on patches included in the
    -- @RejectedPackages@ list. A patch can be allowed only if it is a
    -- dependency of another package, or blocked entirely along with packages
    -- that include it as a dependency.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    -- | Information about the patches to use to update the managed nodes,
    -- including target operating systems and source repositories. Applies to
    -- Linux managed nodes only.
    sources :: Prelude.Maybe [PatchSource]
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
-- 'approvalRules', 'baselineOverride_approvalRules' - Undocumented member.
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
-- 'approvedPatchesEnableNonSecurity', 'baselineOverride_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
--
-- 'globalFilters', 'baselineOverride_globalFilters' - Undocumented member.
--
-- 'operatingSystem', 'baselineOverride_operatingSystem' - The operating system rule used by the patch baseline override.
--
-- 'rejectedPatches', 'baselineOverride_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'rejectedPatchesAction', 'baselineOverride_rejectedPatchesAction' - The action for Patch Manager to take on patches included in the
-- @RejectedPackages@ list. A patch can be allowed only if it is a
-- dependency of another package, or blocked entirely along with packages
-- that include it as a dependency.
--
-- 'sources', 'baselineOverride_sources' - Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
newBaselineOverride ::
  BaselineOverride
newBaselineOverride =
  BaselineOverride'
    { approvalRules = Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      approvedPatchesEnableNonSecurity = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      sources = Prelude.Nothing
    }

-- | Undocumented member.
baselineOverride_approvalRules :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchRuleGroup)
baselineOverride_approvalRules = Lens.lens (\BaselineOverride' {approvalRules} -> approvalRules) (\s@BaselineOverride' {} a -> s {approvalRules = a} :: BaselineOverride)

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

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
baselineOverride_approvedPatchesEnableNonSecurity :: Lens.Lens' BaselineOverride (Prelude.Maybe Prelude.Bool)
baselineOverride_approvedPatchesEnableNonSecurity = Lens.lens (\BaselineOverride' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@BaselineOverride' {} a -> s {approvedPatchesEnableNonSecurity = a} :: BaselineOverride)

-- | Undocumented member.
baselineOverride_globalFilters :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchFilterGroup)
baselineOverride_globalFilters = Lens.lens (\BaselineOverride' {globalFilters} -> globalFilters) (\s@BaselineOverride' {} a -> s {globalFilters = a} :: BaselineOverride)

-- | The operating system rule used by the patch baseline override.
baselineOverride_operatingSystem :: Lens.Lens' BaselineOverride (Prelude.Maybe OperatingSystem)
baselineOverride_operatingSystem = Lens.lens (\BaselineOverride' {operatingSystem} -> operatingSystem) (\s@BaselineOverride' {} a -> s {operatingSystem = a} :: BaselineOverride)

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
baselineOverride_rejectedPatches :: Lens.Lens' BaselineOverride (Prelude.Maybe [Prelude.Text])
baselineOverride_rejectedPatches = Lens.lens (\BaselineOverride' {rejectedPatches} -> rejectedPatches) (\s@BaselineOverride' {} a -> s {rejectedPatches = a} :: BaselineOverride) Prelude.. Lens.mapping Lens.coerced

-- | The action for Patch Manager to take on patches included in the
-- @RejectedPackages@ list. A patch can be allowed only if it is a
-- dependency of another package, or blocked entirely along with packages
-- that include it as a dependency.
baselineOverride_rejectedPatchesAction :: Lens.Lens' BaselineOverride (Prelude.Maybe PatchAction)
baselineOverride_rejectedPatchesAction = Lens.lens (\BaselineOverride' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@BaselineOverride' {} a -> s {rejectedPatchesAction = a} :: BaselineOverride)

-- | Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
baselineOverride_sources :: Lens.Lens' BaselineOverride (Prelude.Maybe [PatchSource])
baselineOverride_sources = Lens.lens (\BaselineOverride' {sources} -> sources) (\s@BaselineOverride' {} a -> s {sources = a} :: BaselineOverride) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable BaselineOverride where
  hashWithSalt _salt BaselineOverride' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRules
      `Prelude.hashWithSalt` approvedPatches
      `Prelude.hashWithSalt` approvedPatchesComplianceLevel
      `Prelude.hashWithSalt` approvedPatchesEnableNonSecurity
      `Prelude.hashWithSalt` globalFilters
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` rejectedPatches
      `Prelude.hashWithSalt` rejectedPatchesAction
      `Prelude.hashWithSalt` sources

instance Prelude.NFData BaselineOverride where
  rnf BaselineOverride' {..} =
    Prelude.rnf approvalRules `Prelude.seq`
      Prelude.rnf approvedPatches `Prelude.seq`
        Prelude.rnf approvedPatchesComplianceLevel `Prelude.seq`
          Prelude.rnf approvedPatchesEnableNonSecurity `Prelude.seq`
            Prelude.rnf globalFilters `Prelude.seq`
              Prelude.rnf operatingSystem `Prelude.seq`
                Prelude.rnf rejectedPatches `Prelude.seq`
                  Prelude.rnf rejectedPatchesAction `Prelude.seq`
                    Prelude.rnf sources

instance Data.ToJSON BaselineOverride where
  toJSON BaselineOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApprovalRules" Data..=) Prelude.<$> approvalRules,
            ("ApprovedPatches" Data..=)
              Prelude.<$> approvedPatches,
            ("ApprovedPatchesComplianceLevel" Data..=)
              Prelude.<$> approvedPatchesComplianceLevel,
            ("ApprovedPatchesEnableNonSecurity" Data..=)
              Prelude.<$> approvedPatchesEnableNonSecurity,
            ("GlobalFilters" Data..=) Prelude.<$> globalFilters,
            ("OperatingSystem" Data..=)
              Prelude.<$> operatingSystem,
            ("RejectedPatches" Data..=)
              Prelude.<$> rejectedPatches,
            ("RejectedPatchesAction" Data..=)
              Prelude.<$> rejectedPatchesAction,
            ("Sources" Data..=) Prelude.<$> sources
          ]
      )
