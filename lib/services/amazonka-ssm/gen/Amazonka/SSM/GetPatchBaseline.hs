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
-- Module      : Amazonka.SSM.GetPatchBaseline
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a patch baseline.
module Amazonka.SSM.GetPatchBaseline
  ( -- * Creating a Request
    GetPatchBaseline (..),
    newGetPatchBaseline,

    -- * Request Lenses
    getPatchBaseline_baselineId,

    -- * Destructuring the Response
    GetPatchBaselineResponse (..),
    newGetPatchBaselineResponse,

    -- * Response Lenses
    getPatchBaselineResponse_operatingSystem,
    getPatchBaselineResponse_approvedPatches,
    getPatchBaselineResponse_approvedPatchesComplianceLevel,
    getPatchBaselineResponse_sources,
    getPatchBaselineResponse_name,
    getPatchBaselineResponse_baselineId,
    getPatchBaselineResponse_approvalRules,
    getPatchBaselineResponse_rejectedPatchesAction,
    getPatchBaselineResponse_description,
    getPatchBaselineResponse_globalFilters,
    getPatchBaselineResponse_createdDate,
    getPatchBaselineResponse_rejectedPatches,
    getPatchBaselineResponse_approvedPatchesEnableNonSecurity,
    getPatchBaselineResponse_patchGroups,
    getPatchBaselineResponse_modifiedDate,
    getPatchBaselineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetPatchBaseline' smart constructor.
data GetPatchBaseline = GetPatchBaseline'
  { -- | The ID of the patch baseline to retrieve.
    --
    -- To retrieve information about an Amazon Web Services managed patch
    -- baseline, specify the full Amazon Resource Name (ARN) of the baseline.
    -- For example, for the baseline @AWS-AmazonLinuxDefaultPatchBaseline@,
    -- specify
    -- @arn:aws:ssm:us-east-2:733109147000:patchbaseline\/pb-0e392de35e7c563b7@
    -- instead of @pb-0e392de35e7c563b7@.
    baselineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'getPatchBaseline_baselineId' - The ID of the patch baseline to retrieve.
--
-- To retrieve information about an Amazon Web Services managed patch
-- baseline, specify the full Amazon Resource Name (ARN) of the baseline.
-- For example, for the baseline @AWS-AmazonLinuxDefaultPatchBaseline@,
-- specify
-- @arn:aws:ssm:us-east-2:733109147000:patchbaseline\/pb-0e392de35e7c563b7@
-- instead of @pb-0e392de35e7c563b7@.
newGetPatchBaseline ::
  -- | 'baselineId'
  Prelude.Text ->
  GetPatchBaseline
newGetPatchBaseline pBaselineId_ =
  GetPatchBaseline' {baselineId = pBaselineId_}

-- | The ID of the patch baseline to retrieve.
--
-- To retrieve information about an Amazon Web Services managed patch
-- baseline, specify the full Amazon Resource Name (ARN) of the baseline.
-- For example, for the baseline @AWS-AmazonLinuxDefaultPatchBaseline@,
-- specify
-- @arn:aws:ssm:us-east-2:733109147000:patchbaseline\/pb-0e392de35e7c563b7@
-- instead of @pb-0e392de35e7c563b7@.
getPatchBaseline_baselineId :: Lens.Lens' GetPatchBaseline Prelude.Text
getPatchBaseline_baselineId = Lens.lens (\GetPatchBaseline' {baselineId} -> baselineId) (\s@GetPatchBaseline' {} a -> s {baselineId = a} :: GetPatchBaseline)

instance Core.AWSRequest GetPatchBaseline where
  type
    AWSResponse GetPatchBaseline =
      GetPatchBaselineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPatchBaselineResponse'
            Prelude.<$> (x Core..?> "OperatingSystem")
            Prelude.<*> ( x Core..?> "ApprovedPatches"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ApprovedPatchesComplianceLevel")
            Prelude.<*> (x Core..?> "Sources" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "BaselineId")
            Prelude.<*> (x Core..?> "ApprovalRules")
            Prelude.<*> (x Core..?> "RejectedPatchesAction")
            Prelude.<*> (x Core..?> "Description")
            Prelude.<*> (x Core..?> "GlobalFilters")
            Prelude.<*> (x Core..?> "CreatedDate")
            Prelude.<*> ( x Core..?> "RejectedPatches"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ApprovedPatchesEnableNonSecurity")
            Prelude.<*> (x Core..?> "PatchGroups" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ModifiedDate")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPatchBaseline where
  hashWithSalt _salt GetPatchBaseline' {..} =
    _salt `Prelude.hashWithSalt` baselineId

instance Prelude.NFData GetPatchBaseline where
  rnf GetPatchBaseline' {..} = Prelude.rnf baselineId

instance Core.ToHeaders GetPatchBaseline where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetPatchBaseline" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetPatchBaseline where
  toJSON GetPatchBaseline' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("BaselineId" Core..= baselineId)]
      )

instance Core.ToPath GetPatchBaseline where
  toPath = Prelude.const "/"

instance Core.ToQuery GetPatchBaseline where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPatchBaselineResponse' smart constructor.
data GetPatchBaselineResponse = GetPatchBaselineResponse'
  { -- | Returns the operating system specified for the patch baseline.
    operatingSystem :: Prelude.Maybe OperatingSystem,
    -- | A list of explicitly approved patches for the baseline.
    approvedPatches :: Prelude.Maybe [Prelude.Text],
    -- | Returns the specified compliance severity level for approved patches in
    -- the patch baseline.
    approvedPatchesComplianceLevel :: Prelude.Maybe PatchComplianceLevel,
    -- | Information about the patches to use to update the managed nodes,
    -- including target operating systems and source repositories. Applies to
    -- Linux managed nodes only.
    sources :: Prelude.Maybe [PatchSource],
    -- | The name of the patch baseline.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the retrieved patch baseline.
    baselineId :: Prelude.Maybe Prelude.Text,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Prelude.Maybe PatchRuleGroup,
    -- | The action specified to take on patches included in the
    -- @RejectedPatches@ list. A patch can be allowed only if it is a
    -- dependency of another package, or blocked entirely along with packages
    -- that include it as a dependency.
    rejectedPatchesAction :: Prelude.Maybe PatchAction,
    -- | A description of the patch baseline.
    description :: Prelude.Maybe Prelude.Text,
    -- | A set of global filters used to exclude patches from the baseline.
    globalFilters :: Prelude.Maybe PatchFilterGroup,
    -- | The date the patch baseline was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | A list of explicitly rejected patches for the baseline.
    rejectedPatches :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the list of approved patches includes non-security
    -- updates that should be applied to the managed nodes. The default value
    -- is @false@. Applies to Linux managed nodes only.
    approvedPatchesEnableNonSecurity :: Prelude.Maybe Prelude.Bool,
    -- | Patch groups included in the patch baseline.
    patchGroups :: Prelude.Maybe [Prelude.Text],
    -- | The date the patch baseline was last modified.
    modifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystem', 'getPatchBaselineResponse_operatingSystem' - Returns the operating system specified for the patch baseline.
--
-- 'approvedPatches', 'getPatchBaselineResponse_approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- 'approvedPatchesComplianceLevel', 'getPatchBaselineResponse_approvedPatchesComplianceLevel' - Returns the specified compliance severity level for approved patches in
-- the patch baseline.
--
-- 'sources', 'getPatchBaselineResponse_sources' - Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
--
-- 'name', 'getPatchBaselineResponse_name' - The name of the patch baseline.
--
-- 'baselineId', 'getPatchBaselineResponse_baselineId' - The ID of the retrieved patch baseline.
--
-- 'approvalRules', 'getPatchBaselineResponse_approvalRules' - A set of rules used to include patches in the baseline.
--
-- 'rejectedPatchesAction', 'getPatchBaselineResponse_rejectedPatchesAction' - The action specified to take on patches included in the
-- @RejectedPatches@ list. A patch can be allowed only if it is a
-- dependency of another package, or blocked entirely along with packages
-- that include it as a dependency.
--
-- 'description', 'getPatchBaselineResponse_description' - A description of the patch baseline.
--
-- 'globalFilters', 'getPatchBaselineResponse_globalFilters' - A set of global filters used to exclude patches from the baseline.
--
-- 'createdDate', 'getPatchBaselineResponse_createdDate' - The date the patch baseline was created.
--
-- 'rejectedPatches', 'getPatchBaselineResponse_rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- 'approvedPatchesEnableNonSecurity', 'getPatchBaselineResponse_approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
--
-- 'patchGroups', 'getPatchBaselineResponse_patchGroups' - Patch groups included in the patch baseline.
--
-- 'modifiedDate', 'getPatchBaselineResponse_modifiedDate' - The date the patch baseline was last modified.
--
-- 'httpStatus', 'getPatchBaselineResponse_httpStatus' - The response's http status code.
newGetPatchBaselineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPatchBaselineResponse
newGetPatchBaselineResponse pHttpStatus_ =
  GetPatchBaselineResponse'
    { operatingSystem =
        Prelude.Nothing,
      approvedPatches = Prelude.Nothing,
      approvedPatchesComplianceLevel = Prelude.Nothing,
      sources = Prelude.Nothing,
      name = Prelude.Nothing,
      baselineId = Prelude.Nothing,
      approvalRules = Prelude.Nothing,
      rejectedPatchesAction = Prelude.Nothing,
      description = Prelude.Nothing,
      globalFilters = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      rejectedPatches = Prelude.Nothing,
      approvedPatchesEnableNonSecurity =
        Prelude.Nothing,
      patchGroups = Prelude.Nothing,
      modifiedDate = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the operating system specified for the patch baseline.
getPatchBaselineResponse_operatingSystem :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe OperatingSystem)
getPatchBaselineResponse_operatingSystem = Lens.lens (\GetPatchBaselineResponse' {operatingSystem} -> operatingSystem) (\s@GetPatchBaselineResponse' {} a -> s {operatingSystem = a} :: GetPatchBaselineResponse)

-- | A list of explicitly approved patches for the baseline.
getPatchBaselineResponse_approvedPatches :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe [Prelude.Text])
getPatchBaselineResponse_approvedPatches = Lens.lens (\GetPatchBaselineResponse' {approvedPatches} -> approvedPatches) (\s@GetPatchBaselineResponse' {} a -> s {approvedPatches = a} :: GetPatchBaselineResponse) Prelude.. Lens.mapping Lens.coerced

-- | Returns the specified compliance severity level for approved patches in
-- the patch baseline.
getPatchBaselineResponse_approvedPatchesComplianceLevel :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe PatchComplianceLevel)
getPatchBaselineResponse_approvedPatchesComplianceLevel = Lens.lens (\GetPatchBaselineResponse' {approvedPatchesComplianceLevel} -> approvedPatchesComplianceLevel) (\s@GetPatchBaselineResponse' {} a -> s {approvedPatchesComplianceLevel = a} :: GetPatchBaselineResponse)

-- | Information about the patches to use to update the managed nodes,
-- including target operating systems and source repositories. Applies to
-- Linux managed nodes only.
getPatchBaselineResponse_sources :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe [PatchSource])
getPatchBaselineResponse_sources = Lens.lens (\GetPatchBaselineResponse' {sources} -> sources) (\s@GetPatchBaselineResponse' {} a -> s {sources = a} :: GetPatchBaselineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the patch baseline.
getPatchBaselineResponse_name :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineResponse_name = Lens.lens (\GetPatchBaselineResponse' {name} -> name) (\s@GetPatchBaselineResponse' {} a -> s {name = a} :: GetPatchBaselineResponse)

-- | The ID of the retrieved patch baseline.
getPatchBaselineResponse_baselineId :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineResponse_baselineId = Lens.lens (\GetPatchBaselineResponse' {baselineId} -> baselineId) (\s@GetPatchBaselineResponse' {} a -> s {baselineId = a} :: GetPatchBaselineResponse)

-- | A set of rules used to include patches in the baseline.
getPatchBaselineResponse_approvalRules :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe PatchRuleGroup)
getPatchBaselineResponse_approvalRules = Lens.lens (\GetPatchBaselineResponse' {approvalRules} -> approvalRules) (\s@GetPatchBaselineResponse' {} a -> s {approvalRules = a} :: GetPatchBaselineResponse)

-- | The action specified to take on patches included in the
-- @RejectedPatches@ list. A patch can be allowed only if it is a
-- dependency of another package, or blocked entirely along with packages
-- that include it as a dependency.
getPatchBaselineResponse_rejectedPatchesAction :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe PatchAction)
getPatchBaselineResponse_rejectedPatchesAction = Lens.lens (\GetPatchBaselineResponse' {rejectedPatchesAction} -> rejectedPatchesAction) (\s@GetPatchBaselineResponse' {} a -> s {rejectedPatchesAction = a} :: GetPatchBaselineResponse)

-- | A description of the patch baseline.
getPatchBaselineResponse_description :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe Prelude.Text)
getPatchBaselineResponse_description = Lens.lens (\GetPatchBaselineResponse' {description} -> description) (\s@GetPatchBaselineResponse' {} a -> s {description = a} :: GetPatchBaselineResponse)

-- | A set of global filters used to exclude patches from the baseline.
getPatchBaselineResponse_globalFilters :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe PatchFilterGroup)
getPatchBaselineResponse_globalFilters = Lens.lens (\GetPatchBaselineResponse' {globalFilters} -> globalFilters) (\s@GetPatchBaselineResponse' {} a -> s {globalFilters = a} :: GetPatchBaselineResponse)

-- | The date the patch baseline was created.
getPatchBaselineResponse_createdDate :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe Prelude.UTCTime)
getPatchBaselineResponse_createdDate = Lens.lens (\GetPatchBaselineResponse' {createdDate} -> createdDate) (\s@GetPatchBaselineResponse' {} a -> s {createdDate = a} :: GetPatchBaselineResponse) Prelude.. Lens.mapping Core._Time

-- | A list of explicitly rejected patches for the baseline.
getPatchBaselineResponse_rejectedPatches :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe [Prelude.Text])
getPatchBaselineResponse_rejectedPatches = Lens.lens (\GetPatchBaselineResponse' {rejectedPatches} -> rejectedPatches) (\s@GetPatchBaselineResponse' {} a -> s {rejectedPatches = a} :: GetPatchBaselineResponse) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the list of approved patches includes non-security
-- updates that should be applied to the managed nodes. The default value
-- is @false@. Applies to Linux managed nodes only.
getPatchBaselineResponse_approvedPatchesEnableNonSecurity :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe Prelude.Bool)
getPatchBaselineResponse_approvedPatchesEnableNonSecurity = Lens.lens (\GetPatchBaselineResponse' {approvedPatchesEnableNonSecurity} -> approvedPatchesEnableNonSecurity) (\s@GetPatchBaselineResponse' {} a -> s {approvedPatchesEnableNonSecurity = a} :: GetPatchBaselineResponse)

-- | Patch groups included in the patch baseline.
getPatchBaselineResponse_patchGroups :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe [Prelude.Text])
getPatchBaselineResponse_patchGroups = Lens.lens (\GetPatchBaselineResponse' {patchGroups} -> patchGroups) (\s@GetPatchBaselineResponse' {} a -> s {patchGroups = a} :: GetPatchBaselineResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date the patch baseline was last modified.
getPatchBaselineResponse_modifiedDate :: Lens.Lens' GetPatchBaselineResponse (Prelude.Maybe Prelude.UTCTime)
getPatchBaselineResponse_modifiedDate = Lens.lens (\GetPatchBaselineResponse' {modifiedDate} -> modifiedDate) (\s@GetPatchBaselineResponse' {} a -> s {modifiedDate = a} :: GetPatchBaselineResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
getPatchBaselineResponse_httpStatus :: Lens.Lens' GetPatchBaselineResponse Prelude.Int
getPatchBaselineResponse_httpStatus = Lens.lens (\GetPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@GetPatchBaselineResponse' {} a -> s {httpStatus = a} :: GetPatchBaselineResponse)

instance Prelude.NFData GetPatchBaselineResponse where
  rnf GetPatchBaselineResponse' {..} =
    Prelude.rnf operatingSystem
      `Prelude.seq` Prelude.rnf approvedPatches
      `Prelude.seq` Prelude.rnf approvedPatchesComplianceLevel
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf baselineId
      `Prelude.seq` Prelude.rnf approvalRules
      `Prelude.seq` Prelude.rnf rejectedPatchesAction
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf globalFilters
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf rejectedPatches
      `Prelude.seq` Prelude.rnf approvedPatchesEnableNonSecurity
      `Prelude.seq` Prelude.rnf patchGroups
      `Prelude.seq` Prelude.rnf modifiedDate
      `Prelude.seq` Prelude.rnf httpStatus
