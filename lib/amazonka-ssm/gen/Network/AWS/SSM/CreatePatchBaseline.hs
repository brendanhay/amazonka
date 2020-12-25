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
    cpbName,
    cpbApprovalRules,
    cpbApprovedPatches,
    cpbApprovedPatchesComplianceLevel,
    cpbApprovedPatchesEnableNonSecurity,
    cpbClientToken,
    cpbDescription,
    cpbGlobalFilters,
    cpbOperatingSystem,
    cpbRejectedPatches,
    cpbRejectedPatchesAction,
    cpbSources,
    cpbTags,

    -- * Destructuring the response
    CreatePatchBaselineResponse (..),
    mkCreatePatchBaselineResponse,

    -- ** Response lenses
    cpbrrsBaselineId,
    cpbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkCreatePatchBaseline' smart constructor.
data CreatePatchBaseline = CreatePatchBaseline'
  { -- | The name of the patch baseline.
    name :: Types.BaselineName,
    -- | A set of rules used to include patches in the baseline.
    approvalRules :: Core.Maybe Types.PatchRuleGroup,
    -- | A list of explicitly approved patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
    approvedPatches :: Core.Maybe [Types.PatchId],
    -- | Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
    approvedPatchesComplianceLevel :: Core.Maybe Types.PatchComplianceLevel,
    -- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Core.Maybe Core.Bool,
    -- | User-provided idempotency token.
    clientToken :: Core.Maybe Types.ClientToken,
    -- | A description of the patch baseline.
    description :: Core.Maybe Types.BaselineDescription,
    -- | A set of global filters used to include patches in the baseline.
    globalFilters :: Core.Maybe Types.PatchFilterGroup,
    -- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | A list of explicitly rejected patches for the baseline.
    --
    -- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
    rejectedPatches :: Core.Maybe [Types.PatchId],
    -- | The action for Patch Manager to take on patches included in the RejectedPackages list.
    --
    --
    --     * __ALLOW_AS_DEPENDENCY__ : A package in the Rejected patches list is installed only if it is a dependency of another package. It is considered compliant with the patch baseline, and its status is reported as /InstalledOther/ . This is the default action if no option is specified.
    --
    --
    --     * __BLOCK__ : Packages in the RejectedPatches list, and packages that include them as dependencies, are not installed under any circumstances. If a package was installed before it was added to the Rejected patches list, it is considered non-compliant with the patch baseline, and its status is reported as /InstalledRejected/ .
    rejectedPatchesAction :: Core.Maybe Types.PatchAction,
    -- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
    sources :: Core.Maybe [Types.PatchSource],
    -- | Optional metadata that you assign to a resource. Tags enable you to categorize a resource in different ways, such as by purpose, owner, or environment. For example, you might want to tag a patch baseline to identify the severity level of patches it specifies and the operating system family it applies to. In this case, you could specify the following key name/value pairs:
    --
    --
    --     * @Key=PatchSeverity,Value=Critical@
    --
    --
    --     * @Key=OS,Value=Windows@
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePatchBaseline' value with any optional fields omitted.
mkCreatePatchBaseline ::
  -- | 'name'
  Types.BaselineName ->
  CreatePatchBaseline
mkCreatePatchBaseline name =
  CreatePatchBaseline'
    { name,
      approvalRules = Core.Nothing,
      approvedPatches = Core.Nothing,
      approvedPatchesComplianceLevel = Core.Nothing,
      approvedPatchesEnableNonSecurity = Core.Nothing,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      globalFilters = Core.Nothing,
      operatingSystem = Core.Nothing,
      rejectedPatches = Core.Nothing,
      rejectedPatchesAction = Core.Nothing,
      sources = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbName :: Lens.Lens' CreatePatchBaseline Types.BaselineName
cpbName = Lens.field @"name"
{-# DEPRECATED cpbName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovalRules :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.PatchRuleGroup)
cpbApprovalRules = Lens.field @"approvalRules"
{-# DEPRECATED cpbApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovedPatches :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Types.PatchId])
cpbApprovedPatches = Lens.field @"approvedPatches"
{-# DEPRECATED cpbApprovedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead." #-}

-- | Defines the compliance level for approved patches. This means that if an approved patch is reported as missing, this is the severity of the compliance violation. The default value is UNSPECIFIED.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovedPatchesComplianceLevel :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.PatchComplianceLevel)
cpbApprovedPatchesComplianceLevel = Lens.field @"approvedPatchesComplianceLevel"
{-# DEPRECATED cpbApprovedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead." #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbApprovedPatchesEnableNonSecurity :: Lens.Lens' CreatePatchBaseline (Core.Maybe Core.Bool)
cpbApprovedPatchesEnableNonSecurity = Lens.field @"approvedPatchesEnableNonSecurity"
{-# DEPRECATED cpbApprovedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead." #-}

-- | User-provided idempotency token.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbClientToken :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.ClientToken)
cpbClientToken = Lens.field @"clientToken"
{-# DEPRECATED cpbClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbDescription :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.BaselineDescription)
cpbDescription = Lens.field @"description"
{-# DEPRECATED cpbDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A set of global filters used to include patches in the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbGlobalFilters :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.PatchFilterGroup)
cpbGlobalFilters = Lens.field @"globalFilters"
{-# DEPRECATED cpbGlobalFilters "Use generic-lens or generic-optics with 'globalFilters' instead." #-}

-- | Defines the operating system the patch baseline applies to. The Default value is WINDOWS.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbOperatingSystem :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.OperatingSystem)
cpbOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED cpbOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbRejectedPatches :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Types.PatchId])
cpbRejectedPatches = Lens.field @"rejectedPatches"
{-# DEPRECATED cpbRejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead." #-}

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
cpbRejectedPatchesAction :: Lens.Lens' CreatePatchBaseline (Core.Maybe Types.PatchAction)
cpbRejectedPatchesAction = Lens.field @"rejectedPatchesAction"
{-# DEPRECATED cpbRejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead." #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbSources :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Types.PatchSource])
cpbSources = Lens.field @"sources"
{-# DEPRECATED cpbSources "Use generic-lens or generic-optics with 'sources' instead." #-}

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
cpbTags :: Lens.Lens' CreatePatchBaseline (Core.Maybe [Types.Tag])
cpbTags = Lens.field @"tags"
{-# DEPRECATED cpbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreatePatchBaseline where
  toJSON CreatePatchBaseline {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("ApprovalRules" Core..=) Core.<$> approvalRules,
            ("ApprovedPatches" Core..=) Core.<$> approvedPatches,
            ("ApprovedPatchesComplianceLevel" Core..=)
              Core.<$> approvedPatchesComplianceLevel,
            ("ApprovedPatchesEnableNonSecurity" Core..=)
              Core.<$> approvedPatchesEnableNonSecurity,
            ("ClientToken" Core..=) Core.<$> clientToken,
            ("Description" Core..=) Core.<$> description,
            ("GlobalFilters" Core..=) Core.<$> globalFilters,
            ("OperatingSystem" Core..=) Core.<$> operatingSystem,
            ("RejectedPatches" Core..=) Core.<$> rejectedPatches,
            ("RejectedPatchesAction" Core..=) Core.<$> rejectedPatchesAction,
            ("Sources" Core..=) Core.<$> sources,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreatePatchBaseline where
  type Rs CreatePatchBaseline = CreatePatchBaselineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.CreatePatchBaseline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePatchBaselineResponse'
            Core.<$> (x Core..:? "BaselineId") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreatePatchBaselineResponse' smart constructor.
data CreatePatchBaselineResponse = CreatePatchBaselineResponse'
  { -- | The ID of the created patch baseline.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreatePatchBaselineResponse' value with any optional fields omitted.
mkCreatePatchBaselineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreatePatchBaselineResponse
mkCreatePatchBaselineResponse responseStatus =
  CreatePatchBaselineResponse'
    { baselineId = Core.Nothing,
      responseStatus
    }

-- | The ID of the created patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbrrsBaselineId :: Lens.Lens' CreatePatchBaselineResponse (Core.Maybe Types.BaselineId)
cpbrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED cpbrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpbrrsResponseStatus :: Lens.Lens' CreatePatchBaselineResponse Core.Int
cpbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cpbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
