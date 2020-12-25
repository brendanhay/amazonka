{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetPatchBaseline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a patch baseline.
module Network.AWS.SSM.GetPatchBaseline
  ( -- * Creating a request
    GetPatchBaseline (..),
    mkGetPatchBaseline,

    -- ** Request lenses
    gpbBaselineId,

    -- * Destructuring the response
    GetPatchBaselineResponse (..),
    mkGetPatchBaselineResponse,

    -- ** Response lenses
    gpbrrsApprovalRules,
    gpbrrsApprovedPatches,
    gpbrrsApprovedPatchesComplianceLevel,
    gpbrrsApprovedPatchesEnableNonSecurity,
    gpbrrsBaselineId,
    gpbrrsCreatedDate,
    gpbrrsDescription,
    gpbrrsGlobalFilters,
    gpbrrsModifiedDate,
    gpbrrsName,
    gpbrrsOperatingSystem,
    gpbrrsPatchGroups,
    gpbrrsRejectedPatches,
    gpbrrsRejectedPatchesAction,
    gpbrrsSources,
    gpbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetPatchBaseline' smart constructor.
newtype GetPatchBaseline = GetPatchBaseline'
  { -- | The ID of the patch baseline to retrieve.
    baselineId :: Types.BaselineId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPatchBaseline' value with any optional fields omitted.
mkGetPatchBaseline ::
  -- | 'baselineId'
  Types.BaselineId ->
  GetPatchBaseline
mkGetPatchBaseline baselineId = GetPatchBaseline' {baselineId}

-- | The ID of the patch baseline to retrieve.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbBaselineId :: Lens.Lens' GetPatchBaseline Types.BaselineId
gpbBaselineId = Lens.field @"baselineId"
{-# DEPRECATED gpbBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

instance Core.FromJSON GetPatchBaseline where
  toJSON GetPatchBaseline {..} =
    Core.object
      (Core.catMaybes [Core.Just ("BaselineId" Core..= baselineId)])

instance Core.AWSRequest GetPatchBaseline where
  type Rs GetPatchBaseline = GetPatchBaselineResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetPatchBaseline")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPatchBaselineResponse'
            Core.<$> (x Core..:? "ApprovalRules")
            Core.<*> (x Core..:? "ApprovedPatches")
            Core.<*> (x Core..:? "ApprovedPatchesComplianceLevel")
            Core.<*> (x Core..:? "ApprovedPatchesEnableNonSecurity")
            Core.<*> (x Core..:? "BaselineId")
            Core.<*> (x Core..:? "CreatedDate")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "GlobalFilters")
            Core.<*> (x Core..:? "ModifiedDate")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "OperatingSystem")
            Core.<*> (x Core..:? "PatchGroups")
            Core.<*> (x Core..:? "RejectedPatches")
            Core.<*> (x Core..:? "RejectedPatchesAction")
            Core.<*> (x Core..:? "Sources")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetPatchBaselineResponse' smart constructor.
data GetPatchBaselineResponse = GetPatchBaselineResponse'
  { -- | A set of rules used to include patches in the baseline.
    approvalRules :: Core.Maybe Types.PatchRuleGroup,
    -- | A list of explicitly approved patches for the baseline.
    approvedPatches :: Core.Maybe [Types.PatchId],
    -- | Returns the specified compliance severity level for approved patches in the patch baseline.
    approvedPatchesComplianceLevel :: Core.Maybe Types.PatchComplianceLevel,
    -- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
    approvedPatchesEnableNonSecurity :: Core.Maybe Core.Bool,
    -- | The ID of the retrieved patch baseline.
    baselineId :: Core.Maybe Types.BaselineId,
    -- | The date the patch baseline was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A description of the patch baseline.
    description :: Core.Maybe Types.BaselineDescription,
    -- | A set of global filters used to exclude patches from the baseline.
    globalFilters :: Core.Maybe Types.PatchFilterGroup,
    -- | The date the patch baseline was last modified.
    modifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | The name of the patch baseline.
    name :: Core.Maybe Types.BaselineName,
    -- | Returns the operating system specified for the patch baseline.
    operatingSystem :: Core.Maybe Types.OperatingSystem,
    -- | Patch groups included in the patch baseline.
    patchGroups :: Core.Maybe [Types.PatchGroup],
    -- | A list of explicitly rejected patches for the baseline.
    rejectedPatches :: Core.Maybe [Types.PatchId],
    -- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
    rejectedPatchesAction :: Core.Maybe Types.PatchAction,
    -- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
    sources :: Core.Maybe [Types.PatchSource],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetPatchBaselineResponse' value with any optional fields omitted.
mkGetPatchBaselineResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetPatchBaselineResponse
mkGetPatchBaselineResponse responseStatus =
  GetPatchBaselineResponse'
    { approvalRules = Core.Nothing,
      approvedPatches = Core.Nothing,
      approvedPatchesComplianceLevel = Core.Nothing,
      approvedPatchesEnableNonSecurity = Core.Nothing,
      baselineId = Core.Nothing,
      createdDate = Core.Nothing,
      description = Core.Nothing,
      globalFilters = Core.Nothing,
      modifiedDate = Core.Nothing,
      name = Core.Nothing,
      operatingSystem = Core.Nothing,
      patchGroups = Core.Nothing,
      rejectedPatches = Core.Nothing,
      rejectedPatchesAction = Core.Nothing,
      sources = Core.Nothing,
      responseStatus
    }

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovalRules :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchRuleGroup)
gpbrrsApprovalRules = Lens.field @"approvalRules"
{-# DEPRECATED gpbrrsApprovalRules "Use generic-lens or generic-optics with 'approvalRules' instead." #-}

-- | A list of explicitly approved patches for the baseline.
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovedPatches :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchId])
gpbrrsApprovedPatches = Lens.field @"approvedPatches"
{-# DEPRECATED gpbrrsApprovedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead." #-}

-- | Returns the specified compliance severity level for approved patches in the patch baseline.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovedPatchesComplianceLevel :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchComplianceLevel)
gpbrrsApprovedPatchesComplianceLevel = Lens.field @"approvedPatchesComplianceLevel"
{-# DEPRECATED gpbrrsApprovedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead." #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovedPatchesEnableNonSecurity :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Core.Bool)
gpbrrsApprovedPatchesEnableNonSecurity = Lens.field @"approvedPatchesEnableNonSecurity"
{-# DEPRECATED gpbrrsApprovedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead." #-}

-- | The ID of the retrieved patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsBaselineId :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.BaselineId)
gpbrrsBaselineId = Lens.field @"baselineId"
{-# DEPRECATED gpbrrsBaselineId "Use generic-lens or generic-optics with 'baselineId' instead." #-}

-- | The date the patch baseline was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsCreatedDate :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Core.NominalDiffTime)
gpbrrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED gpbrrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsDescription :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.BaselineDescription)
gpbrrsDescription = Lens.field @"description"
{-# DEPRECATED gpbrrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A set of global filters used to exclude patches from the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsGlobalFilters :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchFilterGroup)
gpbrrsGlobalFilters = Lens.field @"globalFilters"
{-# DEPRECATED gpbrrsGlobalFilters "Use generic-lens or generic-optics with 'globalFilters' instead." #-}

-- | The date the patch baseline was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsModifiedDate :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Core.NominalDiffTime)
gpbrrsModifiedDate = Lens.field @"modifiedDate"
{-# DEPRECATED gpbrrsModifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead." #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsName :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.BaselineName)
gpbrrsName = Lens.field @"name"
{-# DEPRECATED gpbrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Returns the operating system specified for the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsOperatingSystem :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.OperatingSystem)
gpbrrsOperatingSystem = Lens.field @"operatingSystem"
{-# DEPRECATED gpbrrsOperatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead." #-}

-- | Patch groups included in the patch baseline.
--
-- /Note:/ Consider using 'patchGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsPatchGroups :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchGroup])
gpbrrsPatchGroups = Lens.field @"patchGroups"
{-# DEPRECATED gpbrrsPatchGroups "Use generic-lens or generic-optics with 'patchGroups' instead." #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsRejectedPatches :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchId])
gpbrrsRejectedPatches = Lens.field @"rejectedPatches"
{-# DEPRECATED gpbrrsRejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead." #-}

-- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
--
-- /Note:/ Consider using 'rejectedPatchesAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsRejectedPatchesAction :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchAction)
gpbrrsRejectedPatchesAction = Lens.field @"rejectedPatchesAction"
{-# DEPRECATED gpbrrsRejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead." #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsSources :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchSource])
gpbrrsSources = Lens.field @"sources"
{-# DEPRECATED gpbrrsSources "Use generic-lens or generic-optics with 'sources' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsResponseStatus :: Lens.Lens' GetPatchBaselineResponse Core.Int
gpbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gpbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
