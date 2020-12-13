{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdatePatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing patch baseline. Fields not specified in the request are left unchanged.
module Network.AWS.SSM.UpdatePatchBaseline
  ( -- * Creating a request
    UpdatePatchBaseline (..),
    mkUpdatePatchBaseline,

    -- ** Request lenses
    upbReplace,
    upbApprovalRules,
    upbGlobalFilters,
    upbApprovedPatchesComplianceLevel,
    upbRejectedPatchesAction,
    upbApprovedPatches,
    upbApprovedPatchesEnableNonSecurity,
    upbRejectedPatches,
    upbSources,
    upbName,
    upbDescription,
    upbBaselineId,

    -- * Destructuring the response
    UpdatePatchBaselineResponse (..),
    mkUpdatePatchBaselineResponse,

    -- ** Response lenses
    upbrsApprovalRules,
    upbrsOperatingSystem,
    upbrsGlobalFilters,
    upbrsApprovedPatchesComplianceLevel,
    upbrsRejectedPatchesAction,
    upbrsApprovedPatches,
    upbrsApprovedPatchesEnableNonSecurity,
    upbrsRejectedPatches,
    upbrsSources,
    upbrsCreatedDate,
    upbrsName,
    upbrsModifiedDate,
    upbrsDescription,
    upbrsBaselineId,
    upbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkUpdatePatchBaseline' smart constructor.
data UpdatePatchBaseline = UpdatePatchBaseline'
  { -- | If True, then all fields that are required by the CreatePatchBaseline action are also required for this API request. Optional fields that are not specified are set to null.
    replace :: Lude.Maybe Lude.Bool,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Lude.Maybe PatchRuleGroup,
    -- | A set of global filters used to include patches in the baseline.
    globalFilters :: Lude.Maybe PatchFilterGroup,
    -- | Assigns a new compliance severity level to an existing patch baseline.
    approvedPatchesComplianceLevel :: Lude.Maybe PatchComplianceLevel,
    -- | The action for Patch Manager to take on patches included in the RejectedPackages list.
    --
    --
    --     * __ALLOW_AS_DEPENDENCY__ : A package in the Rejected patches list is installed only if it is a dependency of another package. It is considered compliant with the patch baseline, and its status is reported as /InstalledOther/ . This is the default action if no option is specified.
    --
    --
    --     * __BLOCK__ : Packages in the RejectedPatches list, and packages that include them as dependencies, are not installed under any circumstances. If a package was installed before it was added to the Rejected patches list, it is considered non-compliant with the patch baseline, and its status is reported as /InstalledRejected/ .
    rejectedPatchesAction :: Lude.Maybe PatchAction,
    -- | A list of explicitly approved patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
    approvedPatches :: Lude.Maybe [Lude.Text],
    -- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Lude.Maybe Lude.Bool,
    -- | A list of explicitly rejected patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
    rejectedPatches :: Lude.Maybe [Lude.Text],
    -- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
    sources :: Lude.Maybe [PatchSource],
    -- | The name of the patch baseline.
    name :: Lude.Maybe Lude.Text,
    -- | A description of the patch baseline.
    description :: Lude.Maybe Lude.Text,
    -- | The ID of the patch baseline to update.
    baselineId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePatchBaseline' with the minimum fields required to make a request.
--
-- * 'replace' - If True, then all fields that are required by the CreatePatchBaseline action are also required for this API request. Optional fields that are not specified are set to null.
-- * 'approvalRules' - A set of rules used to include patches in the baseline.
-- * 'globalFilters' - A set of global filters used to include patches in the baseline.
-- * 'approvedPatchesComplianceLevel' - Assigns a new compliance severity level to an existing patch baseline.
-- * 'rejectedPatchesAction' - The action for Patch Manager to take on patches included in the RejectedPackages list.
--
--
--     * __ALLOW_AS_DEPENDENCY__ : A package in the Rejected patches list is installed only if it is a dependency of another package. It is considered compliant with the patch baseline, and its status is reported as /InstalledOther/ . This is the default action if no option is specified.
--
--
--     * __BLOCK__ : Packages in the RejectedPatches list, and packages that include them as dependencies, are not installed under any circumstances. If a package was installed before it was added to the Rejected patches list, it is considered non-compliant with the patch baseline, and its status is reported as /InstalledRejected/ .
--
--
-- * 'approvedPatches' - A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
-- * 'approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
-- * 'rejectedPatches' - A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
-- * 'sources' - Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
-- * 'name' - The name of the patch baseline.
-- * 'description' - A description of the patch baseline.
-- * 'baselineId' - The ID of the patch baseline to update.
mkUpdatePatchBaseline ::
  -- | 'baselineId'
  Lude.Text ->
  UpdatePatchBaseline
mkUpdatePatchBaseline pBaselineId_ =
  UpdatePatchBaseline'
    { replace = Lude.Nothing,
      approvalRules = Lude.Nothing,
      globalFilters = Lude.Nothing,
      approvedPatchesComplianceLevel = Lude.Nothing,
      rejectedPatchesAction = Lude.Nothing,
      approvedPatches = Lude.Nothing,
      approvedPatchesEnableNonSecurity = Lude.Nothing,
      rejectedPatches = Lude.Nothing,
      sources = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing,
      baselineId = pBaselineId_
    }

-- | If True, then all fields that are required by the CreatePatchBaseline action are also required for this API request. Optional fields that are not specified are set to null.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbReplace :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe Lude.Bool)
upbReplace = Lens.lens (replace :: UpdatePatchBaseline -> Lude.Maybe Lude.Bool) (\s a -> s {replace = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbReplace "Use generic-lens or generic-optics with 'replace' instead." #-}

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovalRules :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe PatchRuleGroup)
upbApprovalRules = Lens.lens (approvalRules :: UpdatePatchBaseline -> Lude.Maybe PatchRuleGroup) (\s a -> s {approvalRules = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | A set of global filters used to include patches in the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbGlobalFilters :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe PatchFilterGroup)
upbGlobalFilters = Lens.lens (globalFilters :: UpdatePatchBaseline -> Lude.Maybe PatchFilterGroup) (\s a -> s {globalFilters = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbGlobalFilters "Use generic-lens or generic-optics with 'globalFilters' instead." #-}

-- | Assigns a new compliance severity level to an existing patch baseline.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovedPatchesComplianceLevel :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe PatchComplianceLevel)
upbApprovedPatchesComplianceLevel = Lens.lens (approvedPatchesComplianceLevel :: UpdatePatchBaseline -> Lude.Maybe PatchComplianceLevel) (\s a -> s {approvedPatchesComplianceLevel = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbApprovedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead." #-}

-- | The action for Patch Manager to take on patches included in the RejectedPackages list.
--
--
--     * __ALLOW_AS_DEPENDENCY__ : A package in the Rejected patches list is installed only if it is a dependency of another package. It is considered compliant with the patch baseline, and its status is reported as /InstalledOther/ . This is the default action if no option is specified.
--
--
--     * __BLOCK__ : Packages in the RejectedPatches list, and packages that include them as dependencies, are not installed under any circumstances. If a package was installed before it was added to the Rejected patches list, it is considered non-compliant with the patch baseline, and its status is reported as /InstalledRejected/ .
--
--
--
-- /Note:/ Consider using 'rejectedPatchesAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbRejectedPatchesAction :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe PatchAction)
upbRejectedPatchesAction = Lens.lens (rejectedPatchesAction :: UpdatePatchBaseline -> Lude.Maybe PatchAction) (\s a -> s {rejectedPatchesAction = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbRejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead." #-}

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovedPatches :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe [Lude.Text])
upbApprovedPatches = Lens.lens (approvedPatches :: UpdatePatchBaseline -> Lude.Maybe [Lude.Text]) (\s a -> s {approvedPatches = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbApprovedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead." #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovedPatchesEnableNonSecurity :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe Lude.Bool)
upbApprovedPatchesEnableNonSecurity = Lens.lens (approvedPatchesEnableNonSecurity :: UpdatePatchBaseline -> Lude.Maybe Lude.Bool) (\s a -> s {approvedPatchesEnableNonSecurity = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbApprovedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead." #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbRejectedPatches :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe [Lude.Text])
upbRejectedPatches = Lens.lens (rejectedPatches :: UpdatePatchBaseline -> Lude.Maybe [Lude.Text]) (\s a -> s {rejectedPatches = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbRejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead." #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbSources :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe [PatchSource])
upbSources = Lens.lens (sources :: UpdatePatchBaseline -> Lude.Maybe [PatchSource]) (\s a -> s {sources = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbName :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe Lude.Text)
upbName = Lens.lens (name :: UpdatePatchBaseline -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbDescription :: Lens.Lens' UpdatePatchBaseline (Lude.Maybe Lude.Text)
upbDescription = Lens.lens (description :: UpdatePatchBaseline -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the patch baseline to update.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbBaselineId :: Lens.Lens' UpdatePatchBaseline Lude.Text
upbBaselineId = Lens.lens (baselineId :: UpdatePatchBaseline -> Lude.Text) (\s a -> s {baselineId = a} :: UpdatePatchBaseline)
{-# DEPRECATED upbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Lude.AWSRequest UpdatePatchBaseline where
  type Rs UpdatePatchBaseline = UpdatePatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdatePatchBaselineResponse'
            Lude.<$> (x Lude..?> "ApprovalRules")
            Lude.<*> (x Lude..?> "OperatingSystem")
            Lude.<*> (x Lude..?> "GlobalFilters")
            Lude.<*> (x Lude..?> "ApprovedPatchesComplianceLevel")
            Lude.<*> (x Lude..?> "RejectedPatchesAction")
            Lude.<*> (x Lude..?> "ApprovedPatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "ApprovedPatchesEnableNonSecurity")
            Lude.<*> (x Lude..?> "RejectedPatches" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Sources" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "CreatedDate")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (x Lude..?> "ModifiedDate")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "BaselineId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdatePatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.UpdatePatchBaseline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdatePatchBaseline where
  toJSON UpdatePatchBaseline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Replace" Lude..=) Lude.<$> replace,
            ("ApprovalRules" Lude..=) Lude.<$> approvalRules,
            ("GlobalFilters" Lude..=) Lude.<$> globalFilters,
            ("ApprovedPatchesComplianceLevel" Lude..=)
              Lude.<$> approvedPatchesComplianceLevel,
            ("RejectedPatchesAction" Lude..=) Lude.<$> rejectedPatchesAction,
            ("ApprovedPatches" Lude..=) Lude.<$> approvedPatches,
            ("ApprovedPatchesEnableNonSecurity" Lude..=)
              Lude.<$> approvedPatchesEnableNonSecurity,
            ("RejectedPatches" Lude..=) Lude.<$> rejectedPatches,
            ("Sources" Lude..=) Lude.<$> sources,
            ("Name" Lude..=) Lude.<$> name,
            ("Description" Lude..=) Lude.<$> description,
            Lude.Just ("BaselineId" Lude..= baselineId)
          ]
      )

instance Lude.ToPath UpdatePatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdatePatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdatePatchBaselineResponse' smart constructor.
data UpdatePatchBaselineResponse = UpdatePatchBaselineResponse'
  { -- | A set of rules used to include patches in the baseline.
    approvalRules :: Lude.Maybe PatchRuleGroup,
    -- | The operating system rule used by the updated patch baseline.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | A set of global filters used to exclude patches from the baseline.
    globalFilters :: Lude.Maybe PatchFilterGroup,
    -- | The compliance severity level assigned to the patch baseline after the update completed.
    approvedPatchesComplianceLevel :: Lude.Maybe PatchComplianceLevel,
    -- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
    rejectedPatchesAction :: Lude.Maybe PatchAction,
    -- | A list of explicitly approved patches for the baseline.
    approvedPatches :: Lude.Maybe [Lude.Text],
    -- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Lude.Maybe Lude.Bool,
    -- | A list of explicitly rejected patches for the baseline.
    rejectedPatches :: Lude.Maybe [Lude.Text],
    -- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
    sources :: Lude.Maybe [PatchSource],
    -- | The date when the patch baseline was created.
    createdDate :: Lude.Maybe Lude.Timestamp,
    -- | The name of the patch baseline.
    name :: Lude.Maybe Lude.Text,
    -- | The date when the patch baseline was last modified.
    modifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | A description of the Patch Baseline.
    description :: Lude.Maybe Lude.Text,
    -- | The ID of the deleted patch baseline.
    baselineId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdatePatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'approvalRules' - A set of rules used to include patches in the baseline.
-- * 'operatingSystem' - The operating system rule used by the updated patch baseline.
-- * 'globalFilters' - A set of global filters used to exclude patches from the baseline.
-- * 'approvedPatchesComplianceLevel' - The compliance severity level assigned to the patch baseline after the update completed.
-- * 'rejectedPatchesAction' - The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
-- * 'approvedPatches' - A list of explicitly approved patches for the baseline.
-- * 'approvedPatchesEnableNonSecurity' - Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
-- * 'rejectedPatches' - A list of explicitly rejected patches for the baseline.
-- * 'sources' - Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
-- * 'createdDate' - The date when the patch baseline was created.
-- * 'name' - The name of the patch baseline.
-- * 'modifiedDate' - The date when the patch baseline was last modified.
-- * 'description' - A description of the Patch Baseline.
-- * 'baselineId' - The ID of the deleted patch baseline.
-- * 'responseStatus' - The response status code.
mkUpdatePatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdatePatchBaselineResponse
mkUpdatePatchBaselineResponse pResponseStatus_ =
  UpdatePatchBaselineResponse'
    { approvalRules = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      globalFilters = Lude.Nothing,
      approvedPatchesComplianceLevel = Lude.Nothing,
      rejectedPatchesAction = Lude.Nothing,
      approvedPatches = Lude.Nothing,
      approvedPatchesEnableNonSecurity = Lude.Nothing,
      rejectedPatches = Lude.Nothing,
      sources = Lude.Nothing,
      createdDate = Lude.Nothing,
      name = Lude.Nothing,
      modifiedDate = Lude.Nothing,
      description = Lude.Nothing,
      baselineId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsApprovalRules :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe PatchRuleGroup)
upbrsApprovalRules = Lens.lens (approvalRules :: UpdatePatchBaselineResponse -> Lude.Maybe PatchRuleGroup) (\s a -> s {approvalRules = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | The operating system rule used by the updated patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsOperatingSystem :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe OperatingSystem)
upbrsOperatingSystem = Lens.lens (operatingSystem :: UpdatePatchBaselineResponse -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A set of global filters used to exclude patches from the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsGlobalFilters :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe PatchFilterGroup)
upbrsGlobalFilters = Lens.lens (globalFilters :: UpdatePatchBaselineResponse -> Lude.Maybe PatchFilterGroup) (\s a -> s {globalFilters = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsGlobalFilters "Use generic-lens or generic-optics with 'globalFilters' instead." #-}

-- | The compliance severity level assigned to the patch baseline after the update completed.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsApprovedPatchesComplianceLevel :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe PatchComplianceLevel)
upbrsApprovedPatchesComplianceLevel = Lens.lens (approvedPatchesComplianceLevel :: UpdatePatchBaselineResponse -> Lude.Maybe PatchComplianceLevel) (\s a -> s {approvedPatchesComplianceLevel = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsApprovedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead." #-}

-- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
--
-- /Note:/ Consider using 'rejectedPatchesAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsRejectedPatchesAction :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe PatchAction)
upbrsRejectedPatchesAction = Lens.lens (rejectedPatchesAction :: UpdatePatchBaselineResponse -> Lude.Maybe PatchAction) (\s a -> s {rejectedPatchesAction = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsRejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead." #-}

-- | A list of explicitly approved patches for the baseline.
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsApprovedPatches :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe [Lude.Text])
upbrsApprovedPatches = Lens.lens (approvedPatches :: UpdatePatchBaselineResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {approvedPatches = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsApprovedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead." #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsApprovedPatchesEnableNonSecurity :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe Lude.Bool)
upbrsApprovedPatchesEnableNonSecurity = Lens.lens (approvedPatchesEnableNonSecurity :: UpdatePatchBaselineResponse -> Lude.Maybe Lude.Bool) (\s a -> s {approvedPatchesEnableNonSecurity = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsApprovedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead." #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsRejectedPatches :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe [Lude.Text])
upbrsRejectedPatches = Lens.lens (rejectedPatches :: UpdatePatchBaselineResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {rejectedPatches = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsRejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead." #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsSources :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe [PatchSource])
upbrsSources = Lens.lens (sources :: UpdatePatchBaselineResponse -> Lude.Maybe [PatchSource]) (\s a -> s {sources = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | The date when the patch baseline was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsCreatedDate :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe Lude.Timestamp)
upbrsCreatedDate = Lens.lens (createdDate :: UpdatePatchBaselineResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdDate = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsName :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe Lude.Text)
upbrsName = Lens.lens (name :: UpdatePatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The date when the patch baseline was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsModifiedDate :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe Lude.Timestamp)
upbrsModifiedDate = Lens.lens (modifiedDate :: UpdatePatchBaselineResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {modifiedDate = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsModifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead." #-}

-- | A description of the Patch Baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsDescription :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe Lude.Text)
upbrsDescription = Lens.lens (description :: UpdatePatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ID of the deleted patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsBaselineId :: Lens.Lens' UpdatePatchBaselineResponse (Lude.Maybe Lude.Text)
upbrsBaselineId = Lens.lens (baselineId :: UpdatePatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrsResponseStatus :: Lens.Lens' UpdatePatchBaselineResponse Lude.Int
upbrsResponseStatus = Lens.lens (responseStatus :: UpdatePatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdatePatchBaselineResponse)
{-# DEPRECATED upbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
