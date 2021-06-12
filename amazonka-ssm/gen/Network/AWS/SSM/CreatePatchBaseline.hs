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
-- Module      : Network.AWS.SSM.CreatePatchBaseline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a patch baseline.
--
-- For information about valid key and value pairs in @PatchFilters@ for
-- each supported operating system type, see
-- <http://docs.aws.amazon.com/systems-manager/latest/APIReference/API_PatchFilter.html PatchFilter>.
module Network.AWS.SSM.CreatePatchBaseline
  ( -- * Creating a Request
    CreatePatchBaseline (..),
    newCreatePatchBaseline,

    -- * Request Lenses
    createPatchBaseline_sources,
    createPatchBaseline_rejectedPatches,
    createPatchBaseline_approvedPatchesEnableNonSecurity,
    createPatchBaseline_approvedPatchesComplianceLevel,
    createPatchBaseline_tags,
    createPatchBaseline_description,
    createPatchBaseline_approvedPatches,
    createPatchBaseline_rejectedPatchesAction,
    createPatchBaseline_operatingSystem,
    createPatchBaseline_globalFilters,
    createPatchBaseline_clientToken,
    createPatchBaseline_approvalRules,
    createPatchBaseline_name,

    -- * Destructuring the Response
    CreatePatchBaselineResponse (..),
    newCreatePatchBaselineResponse,

    -- * Response Lenses
    createPatchBaselineResponse_baselineId,
    createPatchBaselineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreatePatchBaseline' smart constructor.
data CreatePatchBaseline = CreatePatchBaseline'
  { -- | Information about the patches to use to update the instances, including
    -- target operating systems and source repositories. Applies to Linux
    -- instances only.
    sources :: Core.Maybe [PatchSource],
    -- | A list of explicitly rejected patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /AWS Systems Manager User Guide/.
    rejectedPatches :: Core.Maybe [Core.Text],
    -- | Indicates whether the list of approved patches includes non-security
    -- updates that should be applied to the instances. The default value is
    -- \'false\'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Core.Maybe Core.Bool,
    -- | Defines the compliance level for approved patches. When an approved
    -- patch is reported as missing, this value describes the severity of the
    -- compliance violation. The default value is UNSPECIFIED.
    approvedPatchesComplianceLevel :: Core.Maybe PatchComplianceLevel,
    -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag a patch baseline to
    -- identify the severity level of patches it specifies and the operating
    -- system family it applies to. In this case, you could specify the
    -- following key name\/value pairs:
    --
    -- -   @Key=PatchSeverity,Value=Critical@
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- To add tags to an existing patch baseline, use the AddTagsToResource
    -- action.
    tags :: Core.Maybe [Tag],
    -- | A description of the patch baseline.
    description :: Core.Maybe Core.Text,
    -- | A list of explicitly approved patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and
    -- rejected patches, see
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
    -- in the /AWS Systems Manager User Guide/.
    approvedPatches :: Core.Maybe [Core.Text],
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
    rejectedPatchesAction :: Core.Maybe PatchAction,
    -- | Defines the operating system the patch baseline applies to. The Default
    -- value is WINDOWS.
    operatingSystem :: Core.Maybe OperatingSystem,
    -- | A set of global filters used to include patches in the baseline.
    globalFilters :: Core.Maybe PatchFilterGroup,
    -- | User-provided idempotency token.
    clientToken :: Core.Maybe Core.Text,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Core.Maybe PatchRuleGroup,
    -- | The name of the patch baseline.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sources', 'createPatchBaseline_sources' - Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
--
-- 'rejectedPatches', 'createPatchBaseline_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
--
-- 'approvedPatchesEnableNonSecurity', 'createPatchBaseline_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
--
-- 'approvedPatchesComplianceLevel', 'createPatchBaseline_approvedPatchesComplianceLevel' - Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation. The default value is UNSPECIFIED.
--
-- 'tags', 'createPatchBaseline_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a patch baseline to
-- identify the severity level of patches it specifies and the operating
-- system family it applies to. In this case, you could specify the
-- following key name\/value pairs:
--
-- -   @Key=PatchSeverity,Value=Critical@
--
-- -   @Key=OS,Value=Windows@
--
-- To add tags to an existing patch baseline, use the AddTagsToResource
-- action.
--
-- 'description', 'createPatchBaseline_description' - A description of the patch baseline.
--
-- 'approvedPatches', 'createPatchBaseline_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
--
-- 'rejectedPatchesAction', 'createPatchBaseline_rejectedPatchesAction' - The action for Patch Manager to take on patches included in the
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
-- 'operatingSystem', 'createPatchBaseline_operatingSystem' - Defines the operating system the patch baseline applies to. The Default
-- value is WINDOWS.
--
-- 'globalFilters', 'createPatchBaseline_globalFilters' - A set of global filters used to include patches in the baseline.
--
-- 'clientToken', 'createPatchBaseline_clientToken' - User-provided idempotency token.
--
-- 'approvalRules', 'createPatchBaseline_approvalRules' - A set of rules used to include patches in the baseline.
--
-- 'name', 'createPatchBaseline_name' - The name of the patch baseline.
newCreatePatchBaseline ::
  -- | 'name'
  Core.Text ->
  CreatePatchBaseline
newCreatePatchBaseline pName_ =
  CreatePatchBaseline'
    { sources = Core.Nothing,
      rejectedPatches = Core.Nothing,
      approvedPatchesEnableNonSecurity = Core.Nothing,
      approvedPatchesComplianceLevel = Core.Nothing,
      tags = Core.Nothing,
      description = Core.Nothing,
      approvedPatches = Core.Nothing,
      rejectedPatchesAction = Core.Nothing,
      operatingSystem = Core.Nothing,
      globalFilters = Core.Nothing,
      clientToken = Core.Nothing,
      approvalRules = Core.Nothing,
      name = pName_
    }

-- | Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
createPatchBaseline_sources :: Lens.Lens' CreatePatchBaseline (Core.Maybe [PatchSource])
createPatchBaseline_sources = Lens.lens (\CreatePatchBaseline' {sources} -> sources) (\s@CreatePatchBaseline' {} a -> s {sources = a} :: CreatePatchBaseline) Core.. Lens.mapping Lens._Coerce

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
createPatchBaseline_rejectedPatches :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Core.Text])
createPatchBaseline_rejectedPatches = Lens.lens (\CreatePatchBaseline' {rejectedPatches} -> rejectedPatches) (\s@CreatePatchBaseline' {} a -> s {rejectedPatches = a} :: CreatePatchBaseline) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
createPatchBaseline_approvedPatchesEnableNonSecurity :: Lens.Lens' CreatePatchBaseline (Core.Maybe Core.Bool)
createPatchBaseline_approvedPatchesEnableNonSecurity = Lens.lens (\CreatePatchBaseline' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@CreatePatchBaseline' {} a -> s {approvedPatchesEnableNonSecurity = a} :: CreatePatchBaseline)

-- | Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation. The default value is UNSPECIFIED.
createPatchBaseline_approvedPatchesComplianceLevel :: Lens.Lens' CreatePatchBaseline (Core.Maybe PatchComplianceLevel)
createPatchBaseline_approvedPatchesComplianceLevel = Lens.lens (\CreatePatchBaseline' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@CreatePatchBaseline' {} a -> s {approvedPatchesComplianceLevel = a} :: CreatePatchBaseline)

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a patch baseline to
-- identify the severity level of patches it specifies and the operating
-- system family it applies to. In this case, you could specify the
-- following key name\/value pairs:
--
-- -   @Key=PatchSeverity,Value=Critical@
--
-- -   @Key=OS,Value=Windows@
--
-- To add tags to an existing patch baseline, use the AddTagsToResource
-- action.
createPatchBaseline_tags :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Tag])
createPatchBaseline_tags = Lens.lens (\CreatePatchBaseline' {tags} -> tags) (\s@CreatePatchBaseline' {} a -> s {tags = a} :: CreatePatchBaseline) Core.. Lens.mapping Lens._Coerce

-- | A description of the patch baseline.
createPatchBaseline_description :: Lens.Lens' CreatePatchBaseline (Core.Maybe Core.Text)
createPatchBaseline_description = Lens.lens (\CreatePatchBaseline' {description} -> description) (\s@CreatePatchBaseline' {} a -> s {description = a} :: CreatePatchBaseline)

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
createPatchBaseline_approvedPatches :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Core.Text])
createPatchBaseline_approvedPatches = Lens.lens (\CreatePatchBaseline' {approvedPatches} -> approvedPatches) (\s@CreatePatchBaseline' {} a -> s {approvedPatches = a} :: CreatePatchBaseline) Core.. Lens.mapping Lens._Coerce

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
createPatchBaseline_rejectedPatchesAction :: Lens.Lens' CreatePatchBaseline (Core.Maybe PatchAction)
createPatchBaseline_rejectedPatchesAction = Lens.lens (\CreatePatchBaseline' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@CreatePatchBaseline' {} a -> s {rejectedPatchesAction = a} :: CreatePatchBaseline)

-- | Defines the operating system the patch baseline applies to. The Default
-- value is WINDOWS.
createPatchBaseline_operatingSystem :: Lens.Lens' CreatePatchBaseline (Core.Maybe OperatingSystem)
createPatchBaseline_operatingSystem = Lens.lens (\CreatePatchBaseline' {operatingSystem} -> operatingSystem) (\s@CreatePatchBaseline' {} a -> s {operatingSystem = a} :: CreatePatchBaseline)

-- | A set of global filters used to include patches in the baseline.
createPatchBaseline_globalFilters :: Lens.Lens' CreatePatchBaseline (Core.Maybe PatchFilterGroup)
createPatchBaseline_globalFilters = Lens.lens (\CreatePatchBaseline' {globalFilters} -> globalFilters) (\s@CreatePatchBaseline' {} a -> s {globalFilters = a} :: CreatePatchBaseline)

-- | User-provided idempotency token.
createPatchBaseline_clientToken :: Lens.Lens' CreatePatchBaseline (Core.Maybe Core.Text)
createPatchBaseline_clientToken = Lens.lens (\CreatePatchBaseline' {clientToken} -> clientToken) (\s@CreatePatchBaseline' {} a -> s {clientToken = a} :: CreatePatchBaseline)

-- | A set of rules used to include patches in the baseline.
createPatchBaseline_approvalRules :: Lens.Lens' CreatePatchBaseline (Core.Maybe PatchRuleGroup)
createPatchBaseline_approvalRules = Lens.lens (\CreatePatchBaseline' {approvalRules} -> approvalRules) (\s@CreatePatchBaseline' {} a -> s {approvalRules = a} :: CreatePatchBaseline)

-- | The name of the patch baseline.
createPatchBaseline_name :: Lens.Lens' CreatePatchBaseline Core.Text
createPatchBaseline_name = Lens.lens (\CreatePatchBaseline' {name} -> name) (\s@CreatePatchBaseline' {} a -> s {name = a} :: CreatePatchBaseline)

instance Core.AWSRequest CreatePatchBaseline where
  type
    AWSResponse CreatePatchBaseline =
      CreatePatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePatchBaselineResponse'
            Core.<$> (x Core..?> "BaselineId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreatePatchBaseline

instance Core.NFData CreatePatchBaseline

instance Core.ToHeaders CreatePatchBaseline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.CreatePatchBaseline" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreatePatchBaseline where
  toJSON CreatePatchBaseline' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Sources" Core..=) Core.<$> sources,
            ("RejectedPatches" Core..=) Core.<$> rejectedPatches,
            ("ApprovedPatchesEnableNonSecurity" Core..=)
              Core.<$> approvedPatchesEnableNonSecurity,
            ("ApprovedPatchesComplianceLevel" Core..=)
              Core.<$> approvedPatchesComplianceLevel,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            ("ApprovedPatches" Core..=) Core.<$> approvedPatches,
            ("RejectedPatchesAction" Core..=)
              Core.<$> rejectedPatchesAction,
            ("OperatingSystem" Core..=) Core.<$> operatingSystem,
            ("GlobalFilters" Core..=) Core.<$> globalFilters,
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("ApprovalRules" Core..=) Core.<$> approvalRules,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreatePatchBaseline where
  toPath = Core.const "/"

instance Core.ToQuery CreatePatchBaseline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreatePatchBaselineResponse' smart constructor.
data CreatePatchBaselineResponse = CreatePatchBaselineResponse'
  { -- | The ID of the created patch baseline.
    baselineId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreatePatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'createPatchBaselineResponse_baselineId' - The ID of the created patch baseline.
--
-- 'httpStatus', 'createPatchBaselineResponse_httpStatus' - The response's http status code.
newCreatePatchBaselineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreatePatchBaselineResponse
newCreatePatchBaselineResponse pHttpStatus_ =
  CreatePatchBaselineResponse'
    { baselineId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the created patch baseline.
createPatchBaselineResponse_baselineId :: Lens.Lens' CreatePatchBaselineResponse (Core.Maybe Core.Text)
createPatchBaselineResponse_baselineId = Lens.lens (\CreatePatchBaselineResponse' {baselineId} -> baselineId) (\s@CreatePatchBaselineResponse' {} a -> s {baselineId = a} :: CreatePatchBaselineResponse)

-- | The response's http status code.
createPatchBaselineResponse_httpStatus :: Lens.Lens' CreatePatchBaselineResponse Core.Int
createPatchBaselineResponse_httpStatus = Lens.lens (\CreatePatchBaselineResponse' {httpStatus} -> httpStatus) (\s@CreatePatchBaselineResponse' {} a -> s {httpStatus = a} :: CreatePatchBaselineResponse)

instance Core.NFData CreatePatchBaselineResponse
