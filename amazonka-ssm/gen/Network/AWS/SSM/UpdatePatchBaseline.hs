{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdatePatchBaseline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing patch baseline. Fields not specified in the request
-- are left unchanged.
--
-- For information about valid key and value pairs in @PatchFilters@ for
-- each supported operating system type, see
-- <http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html PatchFilter>.
module Network.AWS.SSM.UpdatePatchBaseline
  ( -- * Creating a Request
    UpdatePatchBaseline (..),
    newUpdatePatchBaseline,

    -- * Request Lenses
    updatePatchBaseline_sources,
    updatePatchBaseline_rejectedPatches,
    updatePatchBaseline_approvedPatchesEnableNonSecurity,
    updatePatchBaseline_approvedPatchesComplianceLevel,
    updatePatchBaseline_name,
    updatePatchBaseline_replace,
    updatePatchBaseline_description,
    updatePatchBaseline_approvedPatches,
    updatePatchBaseline_rejectedPatchesAction,
    updatePatchBaseline_globalFilters,
    updatePatchBaseline_approvalRules,
    updatePatchBaseline_baselineId,

    -- * Destructuring the Response
    UpdatePatchBaselineResponse (..),
    newUpdatePatchBaselineResponse,

    -- * Response Lenses
    updatePatchBaselineResponse_createdDate,
    updatePatchBaselineResponse_baselineId,
    updatePatchBaselineResponse_sources,
    updatePatchBaselineResponse_rejectedPatches,
    updatePatchBaselineResponse_approvedPatchesEnableNonSecurity,
    updatePatchBaselineResponse_approvedPatchesComplianceLevel,
    updatePatchBaselineResponse_modifiedDate,
    updatePatchBaselineResponse_name,
    updatePatchBaselineResponse_description,
    updatePatchBaselineResponse_approvedPatches,
    updatePatchBaselineResponse_rejectedPatchesAction,
    updatePatchBaselineResponse_operatingSystem,
    updatePatchBaselineResponse_globalFilters,
    updatePatchBaselineResponse_approvalRules,
    updatePatchBaselineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdatePatchBaseline' smart constructor.
data UpdatePatchBaseline = UpdatePatchBaseline'
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
    -- | Assigns a new compliance severity level to an existing patch baseline.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | The name of the patch baseline.
    name :: Prelude.Maybe Prelude.Text,
    -- | If True, then all fields that are required by the CreatePatchBaseline
    -- action are also required for this API request. Optional fields that are
    -- not specified are set to null.
    replace :: Prelude.Maybe Prelude.Bool,
    -- | A description of the patch baseline.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of explicitly approved patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /AWS Systems Manager User Guide/.
    approvedPatches :: Prelude.Maybe [Prelude.Text],
    -- | The action for Patch Manager to take on patches included in the
    -- RejectedPackages list.
    --
    -- -   __ALLOW_AS_DEPENDENCY__: A package in the Rejected patches list is
    --     installed only if it is a dependency of another package. It is
    --     considered compliant with the patch baseline, and its status is
    --     reported as /InstalledOther/. This is the default action if no
    --     option is specified.
    --
    -- -   __BLOCK__: Packages in the RejectedPatches list, and packages that
    --     include them as dependencies, are not installed under any
    --     circumstances. If a package was installed before it was added to the
    --     Rejected patches list, it is considered non-compliant with the patch
    --     baseline, and its status is reported as /InstalledRejected/.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    -- | A set of global filters used to include patches in the baseline.
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Prelude.Maybe PatchRuleGroup,
    -- | The ID of the patch baseline to update.
    baselineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'updatePatchBaseline_sources' - Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
--
-- 'rejectedPatches', 'updatePatchBaseline_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
--
-- 'approvedPatchesEnableNonSecurity', 'updatePatchBaseline_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
--
-- 'approvedPatchesComplianceLevel', 'updatePatchBaseline_approvedPatchesComplianceLevel' - Assigns a new compliance severity level to an existing patch baseline.
--
-- 'name', 'updatePatchBaseline_name' - The name of the patch baseline.
--
-- 'replace', 'updatePatchBaseline_replace' - If True, then all fields that are required by the CreatePatchBaseline
-- action are also required for this API request. Optional fields that are
-- not specified are set to null.
--
-- 'description', 'updatePatchBaseline_description' - A description of the patch baseline.
--
-- 'approvedPatches', 'updatePatchBaseline_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
--
-- 'rejectedPatchesAction', 'updatePatchBaseline_rejectedPatchesAction' - The action for Patch Manager to take on patches included in the
-- RejectedPackages list.
--
-- -   __ALLOW_AS_DEPENDENCY__: A package in the Rejected patches list is
--     installed only if it is a dependency of another package. It is
--     considered compliant with the patch baseline, and its status is
--     reported as /InstalledOther/. This is the default action if no
--     option is specified.
--
-- -   __BLOCK__: Packages in the RejectedPatches list, and packages that
--     include them as dependencies, are not installed under any
--     circumstances. If a package was installed before it was added to the
--     Rejected patches list, it is considered non-compliant with the patch
--     baseline, and its status is reported as /InstalledRejected/.
--
-- 'globalFilters', 'updatePatchBaseline_globalFilters' - A set of global filters used to include patches in the baseline.
--
-- 'approvalRules', 'updatePatchBaseline_approvalRules' - A set of rules used to include patches in the baseline.
--
-- 'baselineId', 'updatePatchBaseline_baselineId' - The ID of the patch baseline to update.
newUpdatePatchBaseline ::
  -- | 'baselineId'
  Prelude.Text ->
  UpdatePatchBaseline
newUpdatePatchBaseline pBaselineId_ =
  UpdatePatchBaseline'
    { sources = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      name = Prelude.Nothing,
      replace = Prelude.Nothing,
      description = Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      approvalRules = Prelude.Nothing,
      baselineId = pBaselineId_
    }

-- | Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
updatePatchBaseline_sources :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe [PatchSource])
updatePatchBaseline_sources = Lens.lens (\UpdatePatchBaseline' {sources} -> sources) (\s@UpdatePatchBaseline' {} a -> s {sources = a} :: UpdatePatchBaseline) Prelude.. Lens.mapping Lens._Coerce

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
updatePatchBaseline_rejectedPatches :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe [Prelude.Text])
updatePatchBaseline_rejectedPatches = Lens.lens (\UpdatePatchBaseline' {rejectedPatches} -> rejectedPatches) (\s@UpdatePatchBaseline' {} a -> s {rejectedPatches = a} :: UpdatePatchBaseline) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
updatePatchBaseline_approvedPatchesEnableNonSecurity :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe Prelude.Bool)
updatePatchBaseline_approvedPatchesEnableNonSecurity = Lens.lens (\UpdatePatchBaseline' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@UpdatePatchBaseline' {} a -> s {approvedPatchesEnableNonSecurity = a} :: UpdatePatchBaseline)

-- | Assigns a new compliance severity level to an existing patch baseline.
updatePatchBaseline_approvedPatchesComplianceLevel :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe PatchComplianceLevel)
updatePatchBaseline_approvedPatchesComplianceLevel = Lens.lens (\UpdatePatchBaseline' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@UpdatePatchBaseline' {} a -> s {approvedPatchesComplianceLevel = a} :: UpdatePatchBaseline)

-- | The name of the patch baseline.
updatePatchBaseline_name :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe Prelude.Text)
updatePatchBaseline_name = Lens.lens (\UpdatePatchBaseline' {name} -> name) (\s@UpdatePatchBaseline' {} a -> s {name = a} :: UpdatePatchBaseline)

-- | If True, then all fields that are required by the CreatePatchBaseline
-- action are also required for this API request. Optional fields that are
-- not specified are set to null.
updatePatchBaseline_replace :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe Prelude.Bool)
updatePatchBaseline_replace = Lens.lens (\UpdatePatchBaseline' {replace} -> replace) (\s@UpdatePatchBaseline' {} a -> s {replace = a} :: UpdatePatchBaseline)

-- | A description of the patch baseline.
updatePatchBaseline_description :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe Prelude.Text)
updatePatchBaseline_description = Lens.lens (\UpdatePatchBaseline' {description} -> description) (\s@UpdatePatchBaseline' {} a -> s {description = a} :: UpdatePatchBaseline)

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
updatePatchBaseline_approvedPatches :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe [Prelude.Text])
updatePatchBaseline_approvedPatches = Lens.lens (\UpdatePatchBaseline' {approvedPatches} -> approvedPatches) (\s@UpdatePatchBaseline' {} a -> s {approvedPatches = a} :: UpdatePatchBaseline) Prelude.. Lens.mapping Lens._Coerce

-- | The action for Patch Manager to take on patches included in the
-- RejectedPackages list.
--
-- -   __ALLOW_AS_DEPENDENCY__: A package in the Rejected patches list is
--     installed only if it is a dependency of another package. It is
--     considered compliant with the patch baseline, and its status is
--     reported as /InstalledOther/. This is the default action if no
--     option is specified.
--
-- -   __BLOCK__: Packages in the RejectedPatches list, and packages that
--     include them as dependencies, are not installed under any
--     circumstances. If a package was installed before it was added to the
--     Rejected patches list, it is considered non-compliant with the patch
--     baseline, and its status is reported as /InstalledRejected/.
updatePatchBaseline_rejectedPatchesAction :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe PatchAction)
updatePatchBaseline_rejectedPatchesAction = Lens.lens (\UpdatePatchBaseline' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@UpdatePatchBaseline' {} a -> s {rejectedPatchesAction = a} :: UpdatePatchBaseline)

-- | A set of global filters used to include patches in the baseline.
updatePatchBaseline_globalFilters :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe PatchFilterGroup)
updatePatchBaseline_globalFilters = Lens.lens (\UpdatePatchBaseline' {globalFilters} -> globalFilters) (\s@UpdatePatchBaseline' {} a -> s {globalFilters = a} :: UpdatePatchBaseline)

-- | A set of rules used to include patches in the baseline.
updatePatchBaseline_approvalRules :: Lens.Lens' UpdatePatchBaseline (Prelude.Maybe PatchRuleGroup)
updatePatchBaseline_approvalRules = Lens.lens (\UpdatePatchBaseline' {approvalRules} -> approvalRules) (\s@UpdatePatchBaseline' {} a -> s {approvalRules = a} :: UpdatePatchBaseline)

-- | The ID of the patch baseline to update.
updatePatchBaseline_baselineId :: Lens.Lens' UpdatePatchBaseline Prelude.Text
updatePatchBaseline_baselineId = Lens.lens (\UpdatePatchBaseline' {baselineId} -> baselineId) (\s@UpdatePatchBaseline' {} a -> s {baselineId = a} :: UpdatePatchBaseline)

instance Core.AWSRequest UpdatePatchBaseline where
  type
    AWSResponse UpdatePatchBaseline =
      UpdatePatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePatchBaselineResponse'
            Prelude.<$> (x Core..?> "CreatedDate")
            Prelude.<*> (x Core..?> "BaselineId")
            Prelude.<*> (x Core..?> "Sources" Core..!@ Prelude.mempty)
            Prelude.<*> ( x Core..?> "RejectedPatches"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ApprovedPatchesEnableNonSecurity")
            Prelude.<*> (x Core..?> "ApprovedPatchesComplianceLevel")
            Prelude.<*> (x Core..?> "ModifiedDate")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> ( x Core..?> "ApprovedPatches"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "RejectedPatchesAction")
            Prelude.<*> (x Core..?> "OperatingSystem")
            Prelude.<*> (x Core..?> "GlobalFilters")
            Prelude.<*> (x Core..?> "ApprovalRules")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePatchBaseline

instance Prelude.NFData UpdatePatchBaseline

instance Core.ToHeaders UpdatePatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.UpdatePatchBaseline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdatePatchBaseline where
  toJSON UpdatePatchBaseline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Sources" Core..=) Prelude.<$> sources,
            ("RejectedPatches" Core..=)
              Prelude.<$> rejectedPatches,
            ("ApprovedPatchesEnableNonSecurity" Core..=)
              Prelude.<$> approvedPatchesEnableNonSecurity,
            ("ApprovedPatchesComplianceLevel" Core..=)
              Prelude.<$> approvedPatchesComplianceLevel,
            ("Name" Core..=) Prelude.<$> name,
            ("Replace" Core..=) Prelude.<$> replace,
            ("Description" Core..=) Prelude.<$> description,
            ("ApprovedPatches" Core..=)
              Prelude.<$> approvedPatches,
            ("RejectedPatchesAction" Core..=)
              Prelude.<$> rejectedPatchesAction,
            ("GlobalFilters" Core..=) Prelude.<$> globalFilters,
            ("ApprovalRules" Core..=) Prelude.<$> approvalRules,
            Prelude.Just ("BaselineId" Core..= baselineId)
          ]
      )

instance Core.ToPath UpdatePatchBaseline where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdatePatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePatchBaselineResponse' smart constructor.
data UpdatePatchBaselineResponse = UpdatePatchBaselineResponse'
  { -- | The date when the patch baseline was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the deleted patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | Information about the patches to use to update the instances, including
    -- target operating systems and source repositories. Applies to Linux
    -- instances only.
    sources :: Prelude.Maybe [PatchSource],
    -- | A list of explicitly rejected patches for the baseline.
    rejectedPatches :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the list of approved patches includes non-security
    -- updates that should be applied to the instances. The default value is
    -- \'false\'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Prelude.Maybe Prelude.Bool,
    -- | The compliance severity level assigned to the patch baseline after the
    -- update completed.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | The date when the patch baseline was last modified.
    modifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the patch baseline.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the Patch Baseline.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of explicitly approved patches for the baseline.
    approvedPatches :: Prelude.Maybe [Prelude.Text],
    -- | The action specified to take on patches included in the RejectedPatches
    -- list. A patch can be allowed only if it is a dependency of another
    -- package, or blocked entirely along with packages that include it as a
    -- dependency.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    -- | The operating system rule used by the updated patch baseline.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A set of global filters used to exclude patches from the baseline.
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Prelude.Maybe PatchRuleGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'updatePatchBaselineResponse_createdDate' - The date when the patch baseline was created.
--
-- 'baselineId', 'updatePatchBaselineResponse_baselineId' - The ID of the deleted patch baseline.
--
-- 'sources', 'updatePatchBaselineResponse_sources' - Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
--
-- 'rejectedPatches', 'updatePatchBaselineResponse_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- 'approvedPatchesEnableNonSecurity', 'updatePatchBaselineResponse_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
--
-- 'approvedPatchesComplianceLevel', 'updatePatchBaselineResponse_approvedPatchesComplianceLevel' - The compliance severity level assigned to the patch baseline after the
-- update completed.
--
-- 'modifiedDate', 'updatePatchBaselineResponse_modifiedDate' - The date when the patch baseline was last modified.
--
-- 'name', 'updatePatchBaselineResponse_name' - The name of the patch baseline.
--
-- 'description', 'updatePatchBaselineResponse_description' - A description of the Patch Baseline.
--
-- 'approvedPatches', 'updatePatchBaselineResponse_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- 'rejectedPatchesAction', 'updatePatchBaselineResponse_rejectedPatchesAction' - The action specified to take on patches included in the RejectedPatches
-- list. A patch can be allowed only if it is a dependency of another
-- package, or blocked entirely along with packages that include it as a
-- dependency.
--
-- 'operatingSystem', 'updatePatchBaselineResponse_operatingSystem' - The operating system rule used by the updated patch baseline.
--
-- 'globalFilters', 'updatePatchBaselineResponse_globalFilters' - A set of global filters used to exclude patches from the baseline.
--
-- 'approvalRules', 'updatePatchBaselineResponse_approvalRules' - A set of rules used to include patches in the baseline.
--
-- 'httpStatus', 'updatePatchBaselineResponse_httpStatus' - The response's http status code.
newUpdatePatchBaselineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePatchBaselineResponse
newUpdatePatchBaselineResponse pHttpStatus_ =
  UpdatePatchBaselineResponse'
    { createdDate =
        Prelude.Nothing,
      baselineId = Prelude.Nothing,
      sources = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity =
        Prelude.Nothing,
      approvedPatchesComplianceLevel =
        Prelude.Nothing,
      modifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      approvalRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date when the patch baseline was created.
updatePatchBaselineResponse_createdDate :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe Prelude.UTCTime)
updatePatchBaselineResponse_createdDate = Lens.lens (\UpdatePatchBaselineResponse' {createdDate} -> createdDate) (\s@UpdatePatchBaselineResponse' {} a -> s {createdDate = a} :: UpdatePatchBaselineResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the deleted patch baseline.
updatePatchBaselineResponse_baselineId :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe Prelude.Text)
updatePatchBaselineResponse_baselineId = Lens.lens (\UpdatePatchBaselineResponse' {baselineId} -> baselineId) (\s@UpdatePatchBaselineResponse' {} a -> s {baselineId = a} :: UpdatePatchBaselineResponse)

-- | Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
updatePatchBaselineResponse_sources :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe [PatchSource])
updatePatchBaselineResponse_sources = Lens.lens (\UpdatePatchBaselineResponse' {sources} -> sources) (\s@UpdatePatchBaselineResponse' {} a -> s {sources = a} :: UpdatePatchBaselineResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of explicitly rejected patches for the baseline.
updatePatchBaselineResponse_rejectedPatches :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe [Prelude.Text])
updatePatchBaselineResponse_rejectedPatches = Lens.lens (\UpdatePatchBaselineResponse' {rejectedPatches} -> rejectedPatches) (\s@UpdatePatchBaselineResponse' {} a -> s {rejectedPatches = a} :: UpdatePatchBaselineResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
updatePatchBaselineResponse_approvedPatchesEnableNonSecurity :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe Prelude.Bool)
updatePatchBaselineResponse_approvedPatchesEnableNonSecurity = Lens.lens (\UpdatePatchBaselineResponse' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@UpdatePatchBaselineResponse' {} a -> s {approvedPatchesEnableNonSecurity = a} :: UpdatePatchBaselineResponse)

-- | The compliance severity level assigned to the patch baseline after the
-- update completed.
updatePatchBaselineResponse_approvedPatchesComplianceLevel :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe PatchComplianceLevel)
updatePatchBaselineResponse_approvedPatchesComplianceLevel = Lens.lens (\UpdatePatchBaselineResponse' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@UpdatePatchBaselineResponse' {} a -> s {approvedPatchesComplianceLevel = a} :: UpdatePatchBaselineResponse)

-- | The date when the patch baseline was last modified.
updatePatchBaselineResponse_modifiedDate :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe Prelude.UTCTime)
updatePatchBaselineResponse_modifiedDate = Lens.lens (\UpdatePatchBaselineResponse' {modifiedDate} -> modifiedDate) (\s@UpdatePatchBaselineResponse' {} a -> s {modifiedDate = a} :: UpdatePatchBaselineResponse) Prelude.. Lens.mapping Core._Time

-- | The name of the patch baseline.
updatePatchBaselineResponse_name :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe Prelude.Text)
updatePatchBaselineResponse_name = Lens.lens (\UpdatePatchBaselineResponse' {name} -> name) (\s@UpdatePatchBaselineResponse' {} a -> s {name = a} :: UpdatePatchBaselineResponse)

-- | A description of the Patch Baseline.
updatePatchBaselineResponse_description :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe Prelude.Text)
updatePatchBaselineResponse_description = Lens.lens (\UpdatePatchBaselineResponse' {description} -> description) (\s@UpdatePatchBaselineResponse' {} a -> s {description = a} :: UpdatePatchBaselineResponse)

-- | A list of explicitly approved patches for the baseline.
updatePatchBaselineResponse_approvedPatches :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe [Prelude.Text])
updatePatchBaselineResponse_approvedPatches = Lens.lens (\UpdatePatchBaselineResponse' {approvedPatches} -> approvedPatches) (\s@UpdatePatchBaselineResponse' {} a -> s {approvedPatches = a} :: UpdatePatchBaselineResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The action specified to take on patches included in the RejectedPatches
-- list. A patch can be allowed only if it is a dependency of another
-- package, or blocked entirely along with packages that include it as a
-- dependency.
updatePatchBaselineResponse_rejectedPatchesAction :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe PatchAction)
updatePatchBaselineResponse_rejectedPatchesAction = Lens.lens (\UpdatePatchBaselineResponse' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@UpdatePatchBaselineResponse' {} a -> s {rejectedPatchesAction = a} :: UpdatePatchBaselineResponse)

-- | The operating system rule used by the updated patch baseline.
updatePatchBaselineResponse_operatingSystem :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe OperatingSystem)
updatePatchBaselineResponse_operatingSystem = Lens.lens (\UpdatePatchBaselineResponse' {operatingSystem} -> operatingSystem) (\s@UpdatePatchBaselineResponse' {} a -> s {operatingSystem = a} :: UpdatePatchBaselineResponse)

-- | A set of global filters used to exclude patches from the baseline.
updatePatchBaselineResponse_globalFilters :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe PatchFilterGroup)
updatePatchBaselineResponse_globalFilters = Lens.lens (\UpdatePatchBaselineResponse' {globalFilters} -> globalFilters) (\s@UpdatePatchBaselineResponse' {} a -> s {globalFilters = a} :: UpdatePatchBaselineResponse)

-- | A set of rules used to include patches in the baseline.
updatePatchBaselineResponse_approvalRules :: Lens.Lens' UpdatePatchBaselineResponse (Prelude.Maybe PatchRuleGroup)
updatePatchBaselineResponse_approvalRules = Lens.lens (\UpdatePatchBaselineResponse' {approvalRules} -> approvalRules) (\s@UpdatePatchBaselineResponse' {} a -> s {approvalRules = a} :: UpdatePatchBaselineResponse)

-- | The response's http status code.
updatePatchBaselineResponse_httpStatus :: Lens.Lens' UpdatePatchBaselineResponse Prelude.Int
updatePatchBaselineResponse_httpStatus = Lens.lens (\UpdatePatchBaselineResponse' {httpStatus} -> httpStatus) (\s@UpdatePatchBaselineResponse' {} a -> s {httpStatus = a} :: UpdatePatchBaselineResponse)

instance Prelude.NFData UpdatePatchBaselineResponse
