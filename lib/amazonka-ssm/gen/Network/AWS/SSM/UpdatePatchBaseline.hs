{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdatePatchBaseline (..)
    , mkUpdatePatchBaseline
    -- ** Request lenses
    , upbBaselineId
    , upbApprovalRules
    , upbApprovedPatches
    , upbApprovedPatchesComplianceLevel
    , upbApprovedPatchesEnableNonSecurity
    , upbDescription
    , upbGlobalFilters
    , upbName
    , upbRejectedPatches
    , upbRejectedPatchesAction
    , upbReplace
    , upbSources

    -- * Destructuring the response
    , UpdatePatchBaselineResponse (..)
    , mkUpdatePatchBaselineResponse
    -- ** Response lenses
    , upbrrsApprovalRules
    , upbrrsApprovedPatches
    , upbrrsApprovedPatchesComplianceLevel
    , upbrrsApprovedPatchesEnableNonSecurity
    , upbrrsBaselineId
    , upbrrsCreatedDate
    , upbrrsDescription
    , upbrrsGlobalFilters
    , upbrrsModifiedDate
    , upbrrsName
    , upbrrsOperatingSystem
    , upbrrsRejectedPatches
    , upbrrsRejectedPatchesAction
    , upbrrsSources
    , upbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdatePatchBaseline' smart constructor.
data UpdatePatchBaseline = UpdatePatchBaseline'
  { baselineId :: Types.BaselineId
    -- ^ The ID of the patch baseline to update.
  , approvalRules :: Core.Maybe Types.PatchRuleGroup
    -- ^ A set of rules used to include patches in the baseline.
  , approvedPatches :: Core.Maybe [Types.PatchId]
    -- ^ A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
  , approvedPatchesComplianceLevel :: Core.Maybe Types.PatchComplianceLevel
    -- ^ Assigns a new compliance severity level to an existing patch baseline.
  , approvedPatchesEnableNonSecurity :: Core.Maybe Core.Bool
    -- ^ Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
  , description :: Core.Maybe Types.BaselineDescription
    -- ^ A description of the patch baseline.
  , globalFilters :: Core.Maybe Types.PatchFilterGroup
    -- ^ A set of global filters used to include patches in the baseline.
  , name :: Core.Maybe Types.BaselineName
    -- ^ The name of the patch baseline.
  , rejectedPatches :: Core.Maybe [Types.PatchId]
    -- ^ A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
  , rejectedPatchesAction :: Core.Maybe Types.PatchAction
    -- ^ The action for Patch Manager to take on patches included in the RejectedPackages list.
--
--
--     * __ALLOW_AS_DEPENDENCY__ : A package in the Rejected patches list is installed only if it is a dependency of another package. It is considered compliant with the patch baseline, and its status is reported as /InstalledOther/ . This is the default action if no option is specified.
--
--
--     * __BLOCK__ : Packages in the RejectedPatches list, and packages that include them as dependencies, are not installed under any circumstances. If a package was installed before it was added to the Rejected patches list, it is considered non-compliant with the patch baseline, and its status is reported as /InstalledRejected/ .
--
--
  , replace :: Core.Maybe Core.Bool
    -- ^ If True, then all fields that are required by the CreatePatchBaseline action are also required for this API request. Optional fields that are not specified are set to null.
  , sources :: Core.Maybe [Types.PatchSource]
    -- ^ Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdatePatchBaseline' value with any optional fields omitted.
mkUpdatePatchBaseline
    :: Types.BaselineId -- ^ 'baselineId'
    -> UpdatePatchBaseline
mkUpdatePatchBaseline baselineId
  = UpdatePatchBaseline'{baselineId, approvalRules = Core.Nothing,
                         approvedPatches = Core.Nothing,
                         approvedPatchesComplianceLevel = Core.Nothing,
                         approvedPatchesEnableNonSecurity = Core.Nothing,
                         description = Core.Nothing, globalFilters = Core.Nothing,
                         name = Core.Nothing, rejectedPatches = Core.Nothing,
                         rejectedPatchesAction = Core.Nothing, replace = Core.Nothing,
                         sources = Core.Nothing}

-- | The ID of the patch baseline to update.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbBaselineId :: Lens.Lens' UpdatePatchBaseline Types.BaselineId
upbBaselineId = Lens.field @"baselineId"
{-# INLINEABLE upbBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovalRules :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Types.PatchRuleGroup)
upbApprovalRules = Lens.field @"approvalRules"
{-# INLINEABLE upbApprovalRules #-}
{-# DEPRECATED approvalRules "Use generic-lens or generic-optics with 'approvalRules' instead"  #-}

-- | A list of explicitly approved patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovedPatches :: Lens.Lens' UpdatePatchBaseline (Core.Maybe [Types.PatchId])
upbApprovedPatches = Lens.field @"approvedPatches"
{-# INLINEABLE upbApprovedPatches #-}
{-# DEPRECATED approvedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead"  #-}

-- | Assigns a new compliance severity level to an existing patch baseline.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovedPatchesComplianceLevel :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Types.PatchComplianceLevel)
upbApprovedPatchesComplianceLevel = Lens.field @"approvedPatchesComplianceLevel"
{-# INLINEABLE upbApprovedPatchesComplianceLevel #-}
{-# DEPRECATED approvedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead"  #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbApprovedPatchesEnableNonSecurity :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Core.Bool)
upbApprovedPatchesEnableNonSecurity = Lens.field @"approvedPatchesEnableNonSecurity"
{-# INLINEABLE upbApprovedPatchesEnableNonSecurity #-}
{-# DEPRECATED approvedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead"  #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbDescription :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Types.BaselineDescription)
upbDescription = Lens.field @"description"
{-# INLINEABLE upbDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A set of global filters used to include patches in the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbGlobalFilters :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Types.PatchFilterGroup)
upbGlobalFilters = Lens.field @"globalFilters"
{-# INLINEABLE upbGlobalFilters #-}
{-# DEPRECATED globalFilters "Use generic-lens or generic-optics with 'globalFilters' instead"  #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbName :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Types.BaselineName)
upbName = Lens.field @"name"
{-# INLINEABLE upbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- For information about accepted formats for lists of approved patches and rejected patches, see <https://docs.aws.amazon.com/systems-manager/latest/userguide/patch-manager-approved-rejected-package-name-formats.html About package name formats for approved and rejected patch lists> in the /AWS Systems Manager User Guide/ .
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbRejectedPatches :: Lens.Lens' UpdatePatchBaseline (Core.Maybe [Types.PatchId])
upbRejectedPatches = Lens.field @"rejectedPatches"
{-# INLINEABLE upbRejectedPatches #-}
{-# DEPRECATED rejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead"  #-}

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
upbRejectedPatchesAction :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Types.PatchAction)
upbRejectedPatchesAction = Lens.field @"rejectedPatchesAction"
{-# INLINEABLE upbRejectedPatchesAction #-}
{-# DEPRECATED rejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead"  #-}

-- | If True, then all fields that are required by the CreatePatchBaseline action are also required for this API request. Optional fields that are not specified are set to null.
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbReplace :: Lens.Lens' UpdatePatchBaseline (Core.Maybe Core.Bool)
upbReplace = Lens.field @"replace"
{-# INLINEABLE upbReplace #-}
{-# DEPRECATED replace "Use generic-lens or generic-optics with 'replace' instead"  #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbSources :: Lens.Lens' UpdatePatchBaseline (Core.Maybe [Types.PatchSource])
upbSources = Lens.field @"sources"
{-# INLINEABLE upbSources #-}
{-# DEPRECATED sources "Use generic-lens or generic-optics with 'sources' instead"  #-}

instance Core.ToQuery UpdatePatchBaseline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdatePatchBaseline where
        toHeaders UpdatePatchBaseline{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.UpdatePatchBaseline")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdatePatchBaseline where
        toJSON UpdatePatchBaseline{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("BaselineId" Core..= baselineId),
                  ("ApprovalRules" Core..=) Core.<$> approvalRules,
                  ("ApprovedPatches" Core..=) Core.<$> approvedPatches,
                  ("ApprovedPatchesComplianceLevel" Core..=) Core.<$>
                    approvedPatchesComplianceLevel,
                  ("ApprovedPatchesEnableNonSecurity" Core..=) Core.<$>
                    approvedPatchesEnableNonSecurity,
                  ("Description" Core..=) Core.<$> description,
                  ("GlobalFilters" Core..=) Core.<$> globalFilters,
                  ("Name" Core..=) Core.<$> name,
                  ("RejectedPatches" Core..=) Core.<$> rejectedPatches,
                  ("RejectedPatchesAction" Core..=) Core.<$> rejectedPatchesAction,
                  ("Replace" Core..=) Core.<$> replace,
                  ("Sources" Core..=) Core.<$> sources])

instance Core.AWSRequest UpdatePatchBaseline where
        type Rs UpdatePatchBaseline = UpdatePatchBaselineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdatePatchBaselineResponse' Core.<$>
                   (x Core..:? "ApprovalRules") Core.<*> x Core..:? "ApprovedPatches"
                     Core.<*> x Core..:? "ApprovedPatchesComplianceLevel"
                     Core.<*> x Core..:? "ApprovedPatchesEnableNonSecurity"
                     Core.<*> x Core..:? "BaselineId"
                     Core.<*> x Core..:? "CreatedDate"
                     Core.<*> x Core..:? "Description"
                     Core.<*> x Core..:? "GlobalFilters"
                     Core.<*> x Core..:? "ModifiedDate"
                     Core.<*> x Core..:? "Name"
                     Core.<*> x Core..:? "OperatingSystem"
                     Core.<*> x Core..:? "RejectedPatches"
                     Core.<*> x Core..:? "RejectedPatchesAction"
                     Core.<*> x Core..:? "Sources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdatePatchBaselineResponse' smart constructor.
data UpdatePatchBaselineResponse = UpdatePatchBaselineResponse'
  { approvalRules :: Core.Maybe Types.PatchRuleGroup
    -- ^ A set of rules used to include patches in the baseline.
  , approvedPatches :: Core.Maybe [Types.PatchId]
    -- ^ A list of explicitly approved patches for the baseline.
  , approvedPatchesComplianceLevel :: Core.Maybe Types.PatchComplianceLevel
    -- ^ The compliance severity level assigned to the patch baseline after the update completed.
  , approvedPatchesEnableNonSecurity :: Core.Maybe Core.Bool
    -- ^ Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
  , baselineId :: Core.Maybe Types.BaselineId
    -- ^ The ID of the deleted patch baseline.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the patch baseline was created.
  , description :: Core.Maybe Types.Description
    -- ^ A description of the Patch Baseline.
  , globalFilters :: Core.Maybe Types.PatchFilterGroup
    -- ^ A set of global filters used to exclude patches from the baseline.
  , modifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date when the patch baseline was last modified.
  , name :: Core.Maybe Types.BaselineName
    -- ^ The name of the patch baseline.
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ The operating system rule used by the updated patch baseline.
  , rejectedPatches :: Core.Maybe [Types.PatchId]
    -- ^ A list of explicitly rejected patches for the baseline.
  , rejectedPatchesAction :: Core.Maybe Types.PatchAction
    -- ^ The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
  , sources :: Core.Maybe [Types.PatchSource]
    -- ^ Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UpdatePatchBaselineResponse' value with any optional fields omitted.
mkUpdatePatchBaselineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdatePatchBaselineResponse
mkUpdatePatchBaselineResponse responseStatus
  = UpdatePatchBaselineResponse'{approvalRules = Core.Nothing,
                                 approvedPatches = Core.Nothing,
                                 approvedPatchesComplianceLevel = Core.Nothing,
                                 approvedPatchesEnableNonSecurity = Core.Nothing,
                                 baselineId = Core.Nothing, createdDate = Core.Nothing,
                                 description = Core.Nothing, globalFilters = Core.Nothing,
                                 modifiedDate = Core.Nothing, name = Core.Nothing,
                                 operatingSystem = Core.Nothing, rejectedPatches = Core.Nothing,
                                 rejectedPatchesAction = Core.Nothing, sources = Core.Nothing,
                                 responseStatus}

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsApprovalRules :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.PatchRuleGroup)
upbrrsApprovalRules = Lens.field @"approvalRules"
{-# INLINEABLE upbrrsApprovalRules #-}
{-# DEPRECATED approvalRules "Use generic-lens or generic-optics with 'approvalRules' instead"  #-}

-- | A list of explicitly approved patches for the baseline.
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsApprovedPatches :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe [Types.PatchId])
upbrrsApprovedPatches = Lens.field @"approvedPatches"
{-# INLINEABLE upbrrsApprovedPatches #-}
{-# DEPRECATED approvedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead"  #-}

-- | The compliance severity level assigned to the patch baseline after the update completed.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsApprovedPatchesComplianceLevel :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.PatchComplianceLevel)
upbrrsApprovedPatchesComplianceLevel = Lens.field @"approvedPatchesComplianceLevel"
{-# INLINEABLE upbrrsApprovedPatchesComplianceLevel #-}
{-# DEPRECATED approvedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead"  #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsApprovedPatchesEnableNonSecurity :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Core.Bool)
upbrrsApprovedPatchesEnableNonSecurity = Lens.field @"approvedPatchesEnableNonSecurity"
{-# INLINEABLE upbrrsApprovedPatchesEnableNonSecurity #-}
{-# DEPRECATED approvedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead"  #-}

-- | The ID of the deleted patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsBaselineId :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.BaselineId)
upbrrsBaselineId = Lens.field @"baselineId"
{-# INLINEABLE upbrrsBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | The date when the patch baseline was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsCreatedDate :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Core.NominalDiffTime)
upbrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE upbrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the Patch Baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsDescription :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.Description)
upbrrsDescription = Lens.field @"description"
{-# INLINEABLE upbrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A set of global filters used to exclude patches from the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsGlobalFilters :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.PatchFilterGroup)
upbrrsGlobalFilters = Lens.field @"globalFilters"
{-# INLINEABLE upbrrsGlobalFilters #-}
{-# DEPRECATED globalFilters "Use generic-lens or generic-optics with 'globalFilters' instead"  #-}

-- | The date when the patch baseline was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsModifiedDate :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Core.NominalDiffTime)
upbrrsModifiedDate = Lens.field @"modifiedDate"
{-# INLINEABLE upbrrsModifiedDate #-}
{-# DEPRECATED modifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead"  #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsName :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.BaselineName)
upbrrsName = Lens.field @"name"
{-# INLINEABLE upbrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The operating system rule used by the updated patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsOperatingSystem :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.OperatingSystem)
upbrrsOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE upbrrsOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsRejectedPatches :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe [Types.PatchId])
upbrrsRejectedPatches = Lens.field @"rejectedPatches"
{-# INLINEABLE upbrrsRejectedPatches #-}
{-# DEPRECATED rejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead"  #-}

-- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
--
-- /Note:/ Consider using 'rejectedPatchesAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsRejectedPatchesAction :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe Types.PatchAction)
upbrrsRejectedPatchesAction = Lens.field @"rejectedPatchesAction"
{-# INLINEABLE upbrrsRejectedPatchesAction #-}
{-# DEPRECATED rejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead"  #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsSources :: Lens.Lens' UpdatePatchBaselineResponse (Core.Maybe [Types.PatchSource])
upbrrsSources = Lens.field @"sources"
{-# INLINEABLE upbrrsSources #-}
{-# DEPRECATED sources "Use generic-lens or generic-optics with 'sources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upbrrsResponseStatus :: Lens.Lens' UpdatePatchBaselineResponse Core.Int
upbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE upbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
