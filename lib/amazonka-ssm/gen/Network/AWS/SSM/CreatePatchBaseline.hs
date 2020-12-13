{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.CreatePatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a patch baseline.
module Network.AWS.SSM.CreatePatchBaseline
  ( -- * Creating a request
    CreatePatchBaseline (..),
    mkCreatePatchBaseline,

    -- ** Request lenses
    cpbApprovalRules,
    cpbClientToken,
    cpbOperatingSystem,
    cpbGlobalFilters,
    cpbApprovedPatchesComplianceLevel,
    cpbRejectedPatchesAction,
    cpbApprovedPatches,
    cpbApprovedPatchesEnableNonSecurity,
    cpbRejectedPatches,
    cpbSources,
    cpbName,
    cpbDescription,
    cpbTags,

    -- * Destructuring the response
    CreatePatchBaselineResponse (..),
    mkCreatePatchBaselineResponse,

    -- ** Response lenses
    cpbrsBaselineId,
    cpbrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SSM.Types

-- | /See:/ 'mkCreatePatchBaseline' smart constructor.
data CreatePatchBaseline = CreatePatchBaseline'
  { -- | A set of rules used to include patches in the baseline.
    approvalRules :: Lude.Maybe PatchRuleGroup,
    -- | User-provided idempotency token.
    clientToken :: Lude.Maybe Lude.Text,
    -- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
    operatingSystem :: Lude.Maybe OperatingSystem,
    -- | A set of global filters used to include patches in the baseline.
    globalFilters :: Lude.Maybe PatchFilterGroup,
    -- | Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
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
    name :: Lude.Text,
    -- | A description of the patch baseline.
    description :: Lude.Maybe Lude.Text,
    -- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a patch baseline to identify the severity level of patches it specifies and the operating system family it applies to. In this case, you could specify the following key name/value pairs:
    --
    --
    --     * @Key=PatchSeverity,Value=Critical@
    --
    --
    --     * @Key=OS,Value=Windows@
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePatchBaseline' with the minimum fields required to make a request.
--
-- * 'approvalRules' - A set of rules used to include patches in the baseline.
-- * 'clientToken' - User-provided idempotency token.
-- * 'operatingSystem' - Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
-- * 'globalFilters' - A set of global filters used to include patches in the baseline.
-- * 'approvedPatchesComplianceLevel' - Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
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
-- * 'tags' - Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a patch baseline to identify the severity level of patches it specifies and the operating system family it applies to. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=PatchSeverity,Value=Critical@
--
--
--     * @Key=OS,Value=Windows@
mkCreatePatchBaseline ::
  -- | 'name'
  Lude.Text ->
  CreatePatchBaseline
mkCreatePatchBaseline pName_ =
  CreatePatchBaseline'
    { approvalRules = Lude.Nothing,
      clientToken = Lude.Nothing,
      operatingSystem = Lude.Nothing,
      globalFilters = Lude.Nothing,
      approvedPatchesComplianceLevel = Lude.Nothing,
      rejectedPatchesAction = Lude.Nothing,
      approvedPatches = Lude.Nothing,
      approvedPatchesEnableNonSecurity = Lude.Nothing,
      rejectedPatches = Lude.Nothing,
      sources = Lude.Nothing,
      name = pName_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovalRules :: Lens.Lens' CreatePatchBaseline (Lude.Maybe PatchRuleGroup)
cpbApprovalRules = Lens.lens (approvalRules :: CreatePatchBaseline -> Lude.Maybe PatchRuleGroup) (\s a -> s {approvalRules = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbClientToken :: Lens.Lens' CreatePatchBaseline (Lude.Maybe Lude.Text)
cpbClientToken = Lens.lens (clientToken :: CreatePatchBaseline -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbOperatingSystem :: Lens.Lens' CreatePatchBaseline (Lude.Maybe OperatingSystem)
cpbOperatingSystem = Lens.lens (operatingSystem :: CreatePatchBaseline -> Lude.Maybe OperatingSystem) (\s a -> s {operatingSystem = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A set of global filters used to include patches in the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbGlobalFilters :: Lens.Lens' CreatePatchBaseline (Lude.Maybe PatchFilterGroup)
cpbGlobalFilters = Lens.lens (globalFilters :: CreatePatchBaseline -> Lude.Maybe PatchFilterGroup) (\s a -> s {globalFilters = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbGlobalFilters "Use generic-lens or generic-optics with 'globalFilters' instead." #-}

-- | Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovedPatchesComplianceLevel :: Lens.Lens' CreatePatchBaseline (Lude.Maybe PatchComplianceLevel)
cpbApprovedPatchesComplianceLevel = Lens.lens (approvedPatchesComplianceLevel :: CreatePatchBaseline -> Lude.Maybe PatchComplianceLevel) (\s a -> s {approvedPatchesComplianceLevel = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbApprovedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead." #-}

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
cpbRejectedPatchesAction :: Lens.Lens' CreatePatchBaseline (Lude.Maybe PatchAction)
cpbRejectedPatchesAction = Lens.lens (rejectedPatchesAction :: CreatePatchBaseline -> Lude.Maybe PatchAction) (\s a -> s {rejectedPatchesAction = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbRejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead." #-}

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovedPatches :: Lens.Lens' CreatePatchBaseline (Lude.Maybe [Lude.Text])
cpbApprovedPatches = Lens.lens (approvedPatches :: CreatePatchBaseline -> Lude.Maybe [Lude.Text]) (\s a -> s {approvedPatches = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbApprovedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead." #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovedPatchesEnableNonSecurity :: Lens.Lens' CreatePatchBaseline (Lude.Maybe Lude.Bool)
cpbApprovedPatchesEnableNonSecurity = Lens.lens (approvedPatchesEnableNonSecurity :: CreatePatchBaseline -> Lude.Maybe Lude.Bool) (\s a -> s {approvedPatchesEnableNonSecurity = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbApprovedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead." #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbRejectedPatches :: Lens.Lens' CreatePatchBaseline (Lude.Maybe [Lude.Text])
cpbRejectedPatches = Lens.lens (rejectedPatches :: CreatePatchBaseline -> Lude.Maybe [Lude.Text]) (\s a -> s {rejectedPatches = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbRejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead." #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbSources :: Lens.Lens' CreatePatchBaseline (Lude.Maybe [PatchSource])
cpbSources = Lens.lens (sources :: CreatePatchBaseline -> Lude.Maybe [PatchSource]) (\s a -> s {sources = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbName :: Lens.Lens' CreatePatchBaseline Lude.Text
cpbName = Lens.lens (name :: CreatePatchBaseline -> Lude.Text) (\s a -> s {name = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbDescription :: Lens.Lens' CreatePatchBaseline (Lude.Maybe Lude.Text)
cpbDescription = Lens.lens (description :: CreatePatchBaseline -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a patch baseline to identify the severity level of patches it specifies and the operating system family it applies to. In this case, you could specify the following key name/value pairs:
--
--
--     * @Key=PatchSeverity,Value=Critical@
--
--
--     * @Key=OS,Value=Windows@
--
--
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbTags :: Lens.Lens' CreatePatchBaseline (Lude.Maybe [Tag])
cpbTags = Lens.lens (tags :: CreatePatchBaseline -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreatePatchBaseline)
{-# DEPRECATED cpbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreatePatchBaseline where
  type Rs CreatePatchBaseline = CreatePatchBaselineResponse
  request = Req.postJSON ssmService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreatePatchBaselineResponse'
            Lude.<$> (x Lude..?> "BaselineId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreatePatchBaseline where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonSSM.CreatePatchBaseline" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreatePatchBaseline where
  toJSON CreatePatchBaseline' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ApprovalRules" Lude..=) Lude.<$> approvalRules,
            ("ClientToken" Lude..=) Lude.<$> clientToken,
            ("OperatingSystem" Lude..=) Lude.<$> operatingSystem,
            ("GlobalFilters" Lude..=) Lude.<$> globalFilters,
            ("ApprovedPatchesComplianceLevel" Lude..=)
              Lude.<$> approvedPatchesComplianceLevel,
            ("RejectedPatchesAction" Lude..=) Lude.<$> rejectedPatchesAction,
            ("ApprovedPatches" Lude..=) Lude.<$> approvedPatches,
            ("ApprovedPatchesEnableNonSecurity" Lude..=)
              Lude.<$> approvedPatchesEnableNonSecurity,
            ("RejectedPatches" Lude..=) Lude.<$> rejectedPatches,
            ("Sources" Lude..=) Lude.<$> sources,
            Lude.Just ("Name" Lude..= name),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreatePatchBaseline where
  toPath = Lude.const "/"

instance Lude.ToQuery CreatePatchBaseline where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreatePatchBaselineResponse' smart constructor.
data CreatePatchBaselineResponse = CreatePatchBaselineResponse'
  { -- | The ID of the created patch baseline.
    baselineId :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreatePatchBaselineResponse' with the minimum fields required to make a request.
--
-- * 'baselineId' - The ID of the created patch baseline.
-- * 'responseStatus' - The response status code.
mkCreatePatchBaselineResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreatePatchBaselineResponse
mkCreatePatchBaselineResponse pResponseStatus_ =
  CreatePatchBaselineResponse'
    { baselineId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the created patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbrsBaselineId :: Lens.Lens' CreatePatchBaselineResponse (Lude.Maybe Lude.Text)
cpbrsBaselineId = Lens.lens (baselineId :: CreatePatchBaselineResponse -> Lude.Maybe Lude.Text) (\s a -> s {baselineId = a} :: CreatePatchBaselineResponse)
{-# DEPRECATED cpbrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbrsResponseStatus :: Lens.Lens' CreatePatchBaselineResponse Lude.Int
cpbrsResponseStatus = Lens.lens (responseStatus :: CreatePatchBaselineResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreatePatchBaselineResponse)
{-# DEPRECATED cpbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
