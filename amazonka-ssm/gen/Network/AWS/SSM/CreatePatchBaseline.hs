{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newCreatePatchBaseline' smart constructor.
data CreatePatchBaseline = CreatePatchBaseline'
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
    -- compliance violation. The default value is UNSPECIFIED.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
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
    tags :: Prelude.Maybe [Tag],
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
    -- | Defines the operating system the patch baseline applies to. The Default
    -- value is WINDOWS.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A set of global filters used to include patches in the baseline.
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Prelude.Maybe PatchRuleGroup,
    -- | The name of the patch baseline.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  CreatePatchBaseline
newCreatePatchBaseline pName_ =
  CreatePatchBaseline'
    { sources = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      tags = Prelude.Nothing,
      description = Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      approvalRules = Prelude.Nothing,
      name = pName_
    }

-- | Information about the patches to use to update the instances, including
-- target operating systems and source repositories. Applies to Linux
-- instances only.
createPatchBaseline_sources :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [PatchSource])
createPatchBaseline_sources = Lens.lens (\CreatePatchBaseline' {sources} -> sources) (\s@CreatePatchBaseline' {} a -> s {sources = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
createPatchBaseline_rejectedPatches :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [Prelude.Text])
createPatchBaseline_rejectedPatches = Lens.lens (\CreatePatchBaseline' {rejectedPatches} -> rejectedPatches) (\s@CreatePatchBaseline' {} a -> s {rejectedPatches = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the instances. The default value is
-- \'false\'. Applies to Linux instances only.
createPatchBaseline_approvedPatchesEnableNonSecurity :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe Prelude.Bool)
createPatchBaseline_approvedPatchesEnableNonSecurity = Lens.lens (\CreatePatchBaseline' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@CreatePatchBaseline' {} a -> s {approvedPatchesEnableNonSecurity = a} :: CreatePatchBaseline)

-- | Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation. The default value is UNSPECIFIED.
createPatchBaseline_approvedPatchesComplianceLevel :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchComplianceLevel)
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
createPatchBaseline_tags :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [Tag])
createPatchBaseline_tags = Lens.lens (\CreatePatchBaseline' {tags} -> tags) (\s@CreatePatchBaseline' {} a -> s {tags = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Prelude._Coerce

-- | A description of the patch baseline.
createPatchBaseline_description :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe Prelude.Text)
createPatchBaseline_description = Lens.lens (\CreatePatchBaseline' {description} -> description) (\s@CreatePatchBaseline' {} a -> s {description = a} :: CreatePatchBaseline)

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /AWS Systems Manager User Guide/.
createPatchBaseline_approvedPatches :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [Prelude.Text])
createPatchBaseline_approvedPatches = Lens.lens (\CreatePatchBaseline' {approvedPatches} -> approvedPatches) (\s@CreatePatchBaseline' {} a -> s {approvedPatches = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Prelude._Coerce

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
createPatchBaseline_rejectedPatchesAction :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchAction)
createPatchBaseline_rejectedPatchesAction = Lens.lens (\CreatePatchBaseline' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@CreatePatchBaseline' {} a -> s {rejectedPatchesAction = a} :: CreatePatchBaseline)

-- | Defines the operating system the patch baseline applies to. The Default
-- value is WINDOWS.
createPatchBaseline_operatingSystem :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe OperatingSystem)
createPatchBaseline_operatingSystem = Lens.lens (\CreatePatchBaseline' {operatingSystem} -> operatingSystem) (\s@CreatePatchBaseline' {} a -> s {operatingSystem = a} :: CreatePatchBaseline)

-- | A set of global filters used to include patches in the baseline.
createPatchBaseline_globalFilters :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchFilterGroup)
createPatchBaseline_globalFilters = Lens.lens (\CreatePatchBaseline' {globalFilters} -> globalFilters) (\s@CreatePatchBaseline' {} a -> s {globalFilters = a} :: CreatePatchBaseline)

-- | User-provided idempotency token.
createPatchBaseline_clientToken :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe Prelude.Text)
createPatchBaseline_clientToken = Lens.lens (\CreatePatchBaseline' {clientToken} -> clientToken) (\s@CreatePatchBaseline' {} a -> s {clientToken = a} :: CreatePatchBaseline)

-- | A set of rules used to include patches in the baseline.
createPatchBaseline_approvalRules :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchRuleGroup)
createPatchBaseline_approvalRules = Lens.lens (\CreatePatchBaseline' {approvalRules} -> approvalRules) (\s@CreatePatchBaseline' {} a -> s {approvalRules = a} :: CreatePatchBaseline)

-- | The name of the patch baseline.
createPatchBaseline_name :: Lens.Lens' CreatePatchBaseline Prelude.Text
createPatchBaseline_name = Lens.lens (\CreatePatchBaseline' {name} -> name) (\s@CreatePatchBaseline' {} a -> s {name = a} :: CreatePatchBaseline)

instance Prelude.AWSRequest CreatePatchBaseline where
  type
    Rs CreatePatchBaseline =
      CreatePatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePatchBaselineResponse'
            Prelude.<$> (x Prelude..?> "BaselineId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePatchBaseline

instance Prelude.NFData CreatePatchBaseline

instance Prelude.ToHeaders CreatePatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AmazonSSM.CreatePatchBaseline" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON CreatePatchBaseline where
  toJSON CreatePatchBaseline' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Sources" Prelude..=) Prelude.<$> sources,
            ("RejectedPatches" Prelude..=)
              Prelude.<$> rejectedPatches,
            ("ApprovedPatchesEnableNonSecurity" Prelude..=)
              Prelude.<$> approvedPatchesEnableNonSecurity,
            ("ApprovedPatchesComplianceLevel" Prelude..=)
              Prelude.<$> approvedPatchesComplianceLevel,
            ("Tags" Prelude..=) Prelude.<$> tags,
            ("Description" Prelude..=) Prelude.<$> description,
            ("ApprovedPatches" Prelude..=)
              Prelude.<$> approvedPatches,
            ("RejectedPatchesAction" Prelude..=)
              Prelude.<$> rejectedPatchesAction,
            ("OperatingSystem" Prelude..=)
              Prelude.<$> operatingSystem,
            ("GlobalFilters" Prelude..=)
              Prelude.<$> globalFilters,
            ("ClientToken" Prelude..=) Prelude.<$> clientToken,
            ("ApprovalRules" Prelude..=)
              Prelude.<$> approvalRules,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )

instance Prelude.ToPath CreatePatchBaseline where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CreatePatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePatchBaselineResponse' smart constructor.
data CreatePatchBaselineResponse = CreatePatchBaselineResponse'
  { -- | The ID of the created patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CreatePatchBaselineResponse
newCreatePatchBaselineResponse pHttpStatus_ =
  CreatePatchBaselineResponse'
    { baselineId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the created patch baseline.
createPatchBaselineResponse_baselineId :: Lens.Lens' CreatePatchBaselineResponse (Prelude.Maybe Prelude.Text)
createPatchBaselineResponse_baselineId = Lens.lens (\CreatePatchBaselineResponse' {baselineId} -> baselineId) (\s@CreatePatchBaselineResponse' {} a -> s {baselineId = a} :: CreatePatchBaselineResponse)

-- | The response's http status code.
createPatchBaselineResponse_httpStatus :: Lens.Lens' CreatePatchBaselineResponse Prelude.Int
createPatchBaselineResponse_httpStatus = Lens.lens (\CreatePatchBaselineResponse' {httpStatus} -> httpStatus) (\s@CreatePatchBaselineResponse' {} a -> s {httpStatus = a} :: CreatePatchBaselineResponse)

instance Prelude.NFData CreatePatchBaselineResponse
