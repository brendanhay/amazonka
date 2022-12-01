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
-- Module      : Amazonka.SSM.CreatePatchBaseline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a patch baseline.
--
-- For information about valid key-value pairs in @PatchFilters@ for each
-- supported operating system type, see PatchFilter.
module Amazonka.SSM.CreatePatchBaseline
  ( -- * Creating a Request
    CreatePatchBaseline (..),
    newCreatePatchBaseline,

    -- * Request Lenses
    createPatchBaseline_tags,
    createPatchBaseline_operatingSystem,
    createPatchBaseline_approvedPatches,
    createPatchBaseline_approvedPatchesComplianceLevel,
    createPatchBaseline_sources,
    createPatchBaseline_clientToken,
    createPatchBaseline_approvalRules,
    createPatchBaseline_rejectedPatchesAction,
    createPatchBaseline_description,
    createPatchBaseline_globalFilters,
    createPatchBaseline_rejectedPatches,
    createPatchBaseline_approvedPatchesEnableNonSecurity,
    createPatchBaseline_name,

    -- * Destructuring the Response
    CreatePatchBaselineResponse (..),
    newCreatePatchBaselineResponse,

    -- * Response Lenses
    createPatchBaselineResponse_baselineId,
    createPatchBaselineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newCreatePatchBaseline' smart constructor.
data CreatePatchBaseline = CreatePatchBaseline'
  { -- | Optional metadata that you assign to a resource. Tags enable you to
    -- categorize a resource in different ways, such as by purpose, owner, or
    -- environment. For example, you might want to tag a patch baseline to
    -- identify the severity level of patches it specifies and the operating
    -- system family it applies to. In this case, you could specify the
    -- following key-value pairs:
    --
    -- -   @Key=PatchSeverity,Value=Critical@
    --
    -- -   @Key=OS,Value=Windows@
    --
    -- To add tags to an existing patch baseline, use the AddTagsToResource
    -- operation.
    tags :: Prelude.Maybe [Tag],
    -- | Defines the operating system the patch baseline applies to. The default
    -- value is @WINDOWS@.
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
    -- compliance violation. The default value is @UNSPECIFIED@.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | Information about the patches to use to update the managed nodes,
    -- including target operating systems and source repositories. Applies to
    -- Linux managed nodes only.
    sources :: Prelude.Maybe [PatchSource],
    -- | User-provided idempotency token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Prelude.Maybe PatchRuleGroup,
    -- | The action for Patch Manager to take on patches included in the
    -- @RejectedPackages@ list.
    --
    -- -   __@ALLOW_AS_DEPENDENCY@__ : A package in the @Rejected@ patches list
    --     is installed only if it is a dependency of another package. It is
    --     considered compliant with the patch baseline, and its status is
    --     reported as @InstalledOther@. This is the default action if no
    --     option is specified.
    --
    -- -   __@BLOCK@__ : Packages in the @RejectedPatches@ list, and packages
    --     that include them as dependencies, aren\'t installed under any
    --     circumstances. If a package was installed before it was added to the
    --     Rejected patches list, it is considered non-compliant with the patch
    --     baseline, and its status is reported as @InstalledRejected@.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    -- | A description of the patch baseline.
    description :: Prelude.Maybe Prelude.Text,
    -- | A set of global filters used to include patches in the baseline.
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
    approvedPatchesEnableNonSecurity :: Prelude.Maybe Prelude.Bool,
    -- | The name of the patch baseline.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createPatchBaseline_tags' - Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a patch baseline to
-- identify the severity level of patches it specifies and the operating
-- system family it applies to. In this case, you could specify the
-- following key-value pairs:
--
-- -   @Key=PatchSeverity,Value=Critical@
--
-- -   @Key=OS,Value=Windows@
--
-- To add tags to an existing patch baseline, use the AddTagsToResource
-- operation.
--
-- 'operatingSystem', 'createPatchBaseline_operatingSystem' - Defines the operating system the patch baseline applies to. The default
-- value is @WINDOWS@.
--
-- 'approvedPatches', 'createPatchBaseline_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'approvedPatchesComplianceLevel', 'createPatchBaseline_approvedPatchesComplianceLevel' - Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation. The default value is @UNSPECIFIED@.
--
-- 'sources', 'createPatchBaseline_sources' - Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
--
-- 'clientToken', 'createPatchBaseline_clientToken' - User-provided idempotency token.
--
-- 'approvalRules', 'createPatchBaseline_approvalRules' - A set of rules used to include patches in the baseline.
--
-- 'rejectedPatchesAction', 'createPatchBaseline_rejectedPatchesAction' - The action for Patch Manager to take on patches included in the
-- @RejectedPackages@ list.
--
-- -   __@ALLOW_AS_DEPENDENCY@__ : A package in the @Rejected@ patches list
--     is installed only if it is a dependency of another package. It is
--     considered compliant with the patch baseline, and its status is
--     reported as @InstalledOther@. This is the default action if no
--     option is specified.
--
-- -   __@BLOCK@__ : Packages in the @RejectedPatches@ list, and packages
--     that include them as dependencies, aren\'t installed under any
--     circumstances. If a package was installed before it was added to the
--     Rejected patches list, it is considered non-compliant with the patch
--     baseline, and its status is reported as @InstalledRejected@.
--
-- 'description', 'createPatchBaseline_description' - A description of the patch baseline.
--
-- 'globalFilters', 'createPatchBaseline_globalFilters' - A set of global filters used to include patches in the baseline.
--
-- 'rejectedPatches', 'createPatchBaseline_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'approvedPatchesEnableNonSecurity', 'createPatchBaseline_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
--
-- 'name', 'createPatchBaseline_name' - The name of the patch baseline.
newCreatePatchBaseline ::
  -- | 'name'
  Prelude.Text ->
  CreatePatchBaseline
newCreatePatchBaseline pName_ =
  CreatePatchBaseline'
    { tags = Prelude.Nothing,
      operatingSystem = Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      sources = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      approvalRules = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      description = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity = Prelude.Nothing,
      name = pName_
    }

-- | Optional metadata that you assign to a resource. Tags enable you to
-- categorize a resource in different ways, such as by purpose, owner, or
-- environment. For example, you might want to tag a patch baseline to
-- identify the severity level of patches it specifies and the operating
-- system family it applies to. In this case, you could specify the
-- following key-value pairs:
--
-- -   @Key=PatchSeverity,Value=Critical@
--
-- -   @Key=OS,Value=Windows@
--
-- To add tags to an existing patch baseline, use the AddTagsToResource
-- operation.
createPatchBaseline_tags :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [Tag])
createPatchBaseline_tags = Lens.lens (\CreatePatchBaseline' {tags} -> tags) (\s@CreatePatchBaseline' {} a -> s {tags = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Lens.coerced

-- | Defines the operating system the patch baseline applies to. The default
-- value is @WINDOWS@.
createPatchBaseline_operatingSystem :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe OperatingSystem)
createPatchBaseline_operatingSystem = Lens.lens (\CreatePatchBaseline' {operatingSystem} -> operatingSystem) (\s@CreatePatchBaseline' {} a -> s {operatingSystem = a} :: CreatePatchBaseline)

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
createPatchBaseline_approvedPatches :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [Prelude.Text])
createPatchBaseline_approvedPatches = Lens.lens (\CreatePatchBaseline' {approvedPatches} -> approvedPatches) (\s@CreatePatchBaseline' {} a -> s {approvedPatches = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Lens.coerced

-- | Defines the compliance level for approved patches. When an approved
-- patch is reported as missing, this value describes the severity of the
-- compliance violation. The default value is @UNSPECIFIED@.
createPatchBaseline_approvedPatchesComplianceLevel :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchComplianceLevel)
createPatchBaseline_approvedPatchesComplianceLevel = Lens.lens (\CreatePatchBaseline' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@CreatePatchBaseline' {} a -> s {approvedPatchesComplianceLevel = a} :: CreatePatchBaseline)

-- | Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
createPatchBaseline_sources :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [PatchSource])
createPatchBaseline_sources = Lens.lens (\CreatePatchBaseline' {sources} -> sources) (\s@CreatePatchBaseline' {} a -> s {sources = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Lens.coerced

-- | User-provided idempotency token.
createPatchBaseline_clientToken :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe Prelude.Text)
createPatchBaseline_clientToken = Lens.lens (\CreatePatchBaseline' {clientToken} -> clientToken) (\s@CreatePatchBaseline' {} a -> s {clientToken = a} :: CreatePatchBaseline)

-- | A set of rules used to include patches in the baseline.
createPatchBaseline_approvalRules :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchRuleGroup)
createPatchBaseline_approvalRules = Lens.lens (\CreatePatchBaseline' {approvalRules} -> approvalRules) (\s@CreatePatchBaseline' {} a -> s {approvalRules = a} :: CreatePatchBaseline)

-- | The action for Patch Manager to take on patches included in the
-- @RejectedPackages@ list.
--
-- -   __@ALLOW_AS_DEPENDENCY@__ : A package in the @Rejected@ patches list
--     is installed only if it is a dependency of another package. It is
--     considered compliant with the patch baseline, and its status is
--     reported as @InstalledOther@. This is the default action if no
--     option is specified.
--
-- -   __@BLOCK@__ : Packages in the @RejectedPatches@ list, and packages
--     that include them as dependencies, aren\'t installed under any
--     circumstances. If a package was installed before it was added to the
--     Rejected patches list, it is considered non-compliant with the patch
--     baseline, and its status is reported as @InstalledRejected@.
createPatchBaseline_rejectedPatchesAction :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchAction)
createPatchBaseline_rejectedPatchesAction = Lens.lens (\CreatePatchBaseline' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@CreatePatchBaseline' {} a -> s {rejectedPatchesAction = a} :: CreatePatchBaseline)

-- | A description of the patch baseline.
createPatchBaseline_description :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe Prelude.Text)
createPatchBaseline_description = Lens.lens (\CreatePatchBaseline' {description} -> description) (\s@CreatePatchBaseline' {} a -> s {description = a} :: CreatePatchBaseline)

-- | A set of global filters used to include patches in the baseline.
createPatchBaseline_globalFilters :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe PatchFilterGroup)
createPatchBaseline_globalFilters = Lens.lens (\CreatePatchBaseline' {globalFilters} -> globalFilters) (\s@CreatePatchBaseline' {} a -> s {globalFilters = a} :: CreatePatchBaseline)

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and
-- rejected patches, see
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists>
-- in the /Amazon Web Services Systems Manager User Guide/.
createPatchBaseline_rejectedPatches :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe [Prelude.Text])
createPatchBaseline_rejectedPatches = Lens.lens (\CreatePatchBaseline' {rejectedPatches} -> rejectedPatches) (\s@CreatePatchBaseline' {} a -> s {rejectedPatches = a} :: CreatePatchBaseline) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
createPatchBaseline_approvedPatchesEnableNonSecurity :: Lens.Lens' CreatePatchBaseline (Prelude.Maybe Prelude.Bool)
createPatchBaseline_approvedPatchesEnableNonSecurity = Lens.lens (\CreatePatchBaseline' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@CreatePatchBaseline' {} a -> s {approvedPatchesEnableNonSecurity = a} :: CreatePatchBaseline)

-- | The name of the patch baseline.
createPatchBaseline_name :: Lens.Lens' CreatePatchBaseline Prelude.Text
createPatchBaseline_name = Lens.lens (\CreatePatchBaseline' {name} -> name) (\s@CreatePatchBaseline' {} a -> s {name = a} :: CreatePatchBaseline)

instance Core.AWSRequest CreatePatchBaseline where
  type
    AWSResponse CreatePatchBaseline =
      CreatePatchBaselineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePatchBaselineResponse'
            Prelude.<$> (x Core..?> "BaselineId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePatchBaseline where
  hashWithSalt _salt CreatePatchBaseline' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` operatingSystem
      `Prelude.hashWithSalt` approvedPatches
      `Prelude.hashWithSalt` approvedPatchesComplianceLevel
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` approvalRules
      `Prelude.hashWithSalt` rejectedPatchesAction
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` globalFilters
      `Prelude.hashWithSalt` rejectedPatches
      `Prelude.hashWithSalt` approvedPatchesEnableNonSecurity
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreatePatchBaseline where
  rnf CreatePatchBaseline' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf approvedPatches
      `Prelude.seq` Prelude.rnf approvedPatchesComplianceLevel
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf approvalRules
      `Prelude.seq` Prelude.rnf rejectedPatchesAction
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf globalFilters
      `Prelude.seq` Prelude.rnf rejectedPatches
      `Prelude.seq` Prelude.rnf approvedPatchesEnableNonSecurity
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders CreatePatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.CreatePatchBaseline" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreatePatchBaseline where
  toJSON CreatePatchBaseline' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("OperatingSystem" Core..=)
              Prelude.<$> operatingSystem,
            ("ApprovedPatches" Core..=)
              Prelude.<$> approvedPatches,
            ("ApprovedPatchesComplianceLevel" Core..=)
              Prelude.<$> approvedPatchesComplianceLevel,
            ("Sources" Core..=) Prelude.<$> sources,
            ("ClientToken" Core..=) Prelude.<$> clientToken,
            ("ApprovalRules" Core..=) Prelude.<$> approvalRules,
            ("RejectedPatchesAction" Core..=)
              Prelude.<$> rejectedPatchesAction,
            ("Description" Core..=) Prelude.<$> description,
            ("GlobalFilters" Core..=) Prelude.<$> globalFilters,
            ("RejectedPatches" Core..=)
              Prelude.<$> rejectedPatches,
            ("ApprovedPatchesEnableNonSecurity" Core..=)
              Prelude.<$> approvedPatchesEnableNonSecurity,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath CreatePatchBaseline where
  toPath = Prelude.const "/"

instance Core.ToQuery CreatePatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePatchBaselineResponse' smart constructor.
data CreatePatchBaselineResponse = CreatePatchBaselineResponse'
  { -- | The ID of the created patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData CreatePatchBaselineResponse where
  rnf CreatePatchBaselineResponse' {..} =
    Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf httpStatus
