{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetPatchBaseline (..)
    , mkGetPatchBaseline
    -- ** Request lenses
    , gpbBaselineId

    -- * Destructuring the response
    , GetPatchBaselineResponse (..)
    , mkGetPatchBaselineResponse
    -- ** Response lenses
    , gpbrrsApprovalRules
    , gpbrrsApprovedPatches
    , gpbrrsApprovedPatchesComplianceLevel
    , gpbrrsApprovedPatchesEnableNonSecurity
    , gpbrrsBaselineId
    , gpbrrsCreatedDate
    , gpbrrsDescription
    , gpbrrsGlobalFilters
    , gpbrrsModifiedDate
    , gpbrrsName
    , gpbrrsOperatingSystem
    , gpbrrsPatchGroups
    , gpbrrsRejectedPatches
    , gpbrrsRejectedPatchesAction
    , gpbrrsSources
    , gpbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetPatchBaseline' smart constructor.
newtype GetPatchBaseline = GetPatchBaseline'
  { baselineId :: Types.BaselineId
    -- ^ The ID of the patch baseline to retrieve.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetPatchBaseline' value with any optional fields omitted.
mkGetPatchBaseline
    :: Types.BaselineId -- ^ 'baselineId'
    -> GetPatchBaseline
mkGetPatchBaseline baselineId = GetPatchBaseline'{baselineId}

-- | The ID of the patch baseline to retrieve.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbBaselineId :: Lens.Lens' GetPatchBaseline Types.BaselineId
gpbBaselineId = Lens.field @"baselineId"
{-# INLINEABLE gpbBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

instance Core.ToQuery GetPatchBaseline where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetPatchBaseline where
        toHeaders GetPatchBaseline{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.GetPatchBaseline") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetPatchBaseline where
        toJSON GetPatchBaseline{..}
          = Core.object
              (Core.catMaybes [Core.Just ("BaselineId" Core..= baselineId)])

instance Core.AWSRequest GetPatchBaseline where
        type Rs GetPatchBaseline = GetPatchBaselineResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetPatchBaselineResponse' Core.<$>
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
                     Core.<*> x Core..:? "PatchGroups"
                     Core.<*> x Core..:? "RejectedPatches"
                     Core.<*> x Core..:? "RejectedPatchesAction"
                     Core.<*> x Core..:? "Sources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetPatchBaselineResponse' smart constructor.
data GetPatchBaselineResponse = GetPatchBaselineResponse'
  { approvalRules :: Core.Maybe Types.PatchRuleGroup
    -- ^ A set of rules used to include patches in the baseline.
  , approvedPatches :: Core.Maybe [Types.PatchId]
    -- ^ A list of explicitly approved patches for the baseline.
  , approvedPatchesComplianceLevel :: Core.Maybe Types.PatchComplianceLevel
    -- ^ Returns the specified compliance severity level for approved patches in the patch baseline.
  , approvedPatchesEnableNonSecurity :: Core.Maybe Core.Bool
    -- ^ Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
  , baselineId :: Core.Maybe Types.BaselineId
    -- ^ The ID of the retrieved patch baseline.
  , createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the patch baseline was created.
  , description :: Core.Maybe Types.BaselineDescription
    -- ^ A description of the patch baseline.
  , globalFilters :: Core.Maybe Types.PatchFilterGroup
    -- ^ A set of global filters used to exclude patches from the baseline.
  , modifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the patch baseline was last modified.
  , name :: Core.Maybe Types.BaselineName
    -- ^ The name of the patch baseline.
  , operatingSystem :: Core.Maybe Types.OperatingSystem
    -- ^ Returns the operating system specified for the patch baseline.
  , patchGroups :: Core.Maybe [Types.PatchGroup]
    -- ^ Patch groups included in the patch baseline.
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

-- | Creates a 'GetPatchBaselineResponse' value with any optional fields omitted.
mkGetPatchBaselineResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetPatchBaselineResponse
mkGetPatchBaselineResponse responseStatus
  = GetPatchBaselineResponse'{approvalRules = Core.Nothing,
                              approvedPatches = Core.Nothing,
                              approvedPatchesComplianceLevel = Core.Nothing,
                              approvedPatchesEnableNonSecurity = Core.Nothing,
                              baselineId = Core.Nothing, createdDate = Core.Nothing,
                              description = Core.Nothing, globalFilters = Core.Nothing,
                              modifiedDate = Core.Nothing, name = Core.Nothing,
                              operatingSystem = Core.Nothing, patchGroups = Core.Nothing,
                              rejectedPatches = Core.Nothing,
                              rejectedPatchesAction = Core.Nothing, sources = Core.Nothing,
                              responseStatus}

-- | A set of rules used to include patches in the baseline.
--
-- /Note:/ Consider using 'approvalRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovalRules :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchRuleGroup)
gpbrrsApprovalRules = Lens.field @"approvalRules"
{-# INLINEABLE gpbrrsApprovalRules #-}
{-# DEPRECATED approvalRules "Use generic-lens or generic-optics with 'approvalRules' instead"  #-}

-- | A list of explicitly approved patches for the baseline.
--
-- /Note:/ Consider using 'approvedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovedPatches :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchId])
gpbrrsApprovedPatches = Lens.field @"approvedPatches"
{-# INLINEABLE gpbrrsApprovedPatches #-}
{-# DEPRECATED approvedPatches "Use generic-lens or generic-optics with 'approvedPatches' instead"  #-}

-- | Returns the specified compliance severity level for approved patches in the patch baseline.
--
-- /Note:/ Consider using 'approvedPatchesComplianceLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovedPatchesComplianceLevel :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchComplianceLevel)
gpbrrsApprovedPatchesComplianceLevel = Lens.field @"approvedPatchesComplianceLevel"
{-# INLINEABLE gpbrrsApprovedPatchesComplianceLevel #-}
{-# DEPRECATED approvedPatchesComplianceLevel "Use generic-lens or generic-optics with 'approvedPatchesComplianceLevel' instead"  #-}

-- | Indicates whether the list of approved patches includes non-security updates that should be applied to the instances. The default value is 'false'. Applies to Linux instances only.
--
-- /Note:/ Consider using 'approvedPatchesEnableNonSecurity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsApprovedPatchesEnableNonSecurity :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Core.Bool)
gpbrrsApprovedPatchesEnableNonSecurity = Lens.field @"approvedPatchesEnableNonSecurity"
{-# INLINEABLE gpbrrsApprovedPatchesEnableNonSecurity #-}
{-# DEPRECATED approvedPatchesEnableNonSecurity "Use generic-lens or generic-optics with 'approvedPatchesEnableNonSecurity' instead"  #-}

-- | The ID of the retrieved patch baseline.
--
-- /Note:/ Consider using 'baselineId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsBaselineId :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.BaselineId)
gpbrrsBaselineId = Lens.field @"baselineId"
{-# INLINEABLE gpbrrsBaselineId #-}
{-# DEPRECATED baselineId "Use generic-lens or generic-optics with 'baselineId' instead"  #-}

-- | The date the patch baseline was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsCreatedDate :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Core.NominalDiffTime)
gpbrrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE gpbrrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A description of the patch baseline.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsDescription :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.BaselineDescription)
gpbrrsDescription = Lens.field @"description"
{-# INLINEABLE gpbrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A set of global filters used to exclude patches from the baseline.
--
-- /Note:/ Consider using 'globalFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsGlobalFilters :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchFilterGroup)
gpbrrsGlobalFilters = Lens.field @"globalFilters"
{-# INLINEABLE gpbrrsGlobalFilters #-}
{-# DEPRECATED globalFilters "Use generic-lens or generic-optics with 'globalFilters' instead"  #-}

-- | The date the patch baseline was last modified.
--
-- /Note:/ Consider using 'modifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsModifiedDate :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Core.NominalDiffTime)
gpbrrsModifiedDate = Lens.field @"modifiedDate"
{-# INLINEABLE gpbrrsModifiedDate #-}
{-# DEPRECATED modifiedDate "Use generic-lens or generic-optics with 'modifiedDate' instead"  #-}

-- | The name of the patch baseline.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsName :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.BaselineName)
gpbrrsName = Lens.field @"name"
{-# INLINEABLE gpbrrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | Returns the operating system specified for the patch baseline.
--
-- /Note:/ Consider using 'operatingSystem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsOperatingSystem :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.OperatingSystem)
gpbrrsOperatingSystem = Lens.field @"operatingSystem"
{-# INLINEABLE gpbrrsOperatingSystem #-}
{-# DEPRECATED operatingSystem "Use generic-lens or generic-optics with 'operatingSystem' instead"  #-}

-- | Patch groups included in the patch baseline.
--
-- /Note:/ Consider using 'patchGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsPatchGroups :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchGroup])
gpbrrsPatchGroups = Lens.field @"patchGroups"
{-# INLINEABLE gpbrrsPatchGroups #-}
{-# DEPRECATED patchGroups "Use generic-lens or generic-optics with 'patchGroups' instead"  #-}

-- | A list of explicitly rejected patches for the baseline.
--
-- /Note:/ Consider using 'rejectedPatches' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsRejectedPatches :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchId])
gpbrrsRejectedPatches = Lens.field @"rejectedPatches"
{-# INLINEABLE gpbrrsRejectedPatches #-}
{-# DEPRECATED rejectedPatches "Use generic-lens or generic-optics with 'rejectedPatches' instead"  #-}

-- | The action specified to take on patches included in the RejectedPatches list. A patch can be allowed only if it is a dependency of another package, or blocked entirely along with packages that include it as a dependency.
--
-- /Note:/ Consider using 'rejectedPatchesAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsRejectedPatchesAction :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe Types.PatchAction)
gpbrrsRejectedPatchesAction = Lens.field @"rejectedPatchesAction"
{-# INLINEABLE gpbrrsRejectedPatchesAction #-}
{-# DEPRECATED rejectedPatchesAction "Use generic-lens or generic-optics with 'rejectedPatchesAction' instead"  #-}

-- | Information about the patches to use to update the instances, including target operating systems and source repositories. Applies to Linux instances only.
--
-- /Note:/ Consider using 'sources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsSources :: Lens.Lens' GetPatchBaselineResponse (Core.Maybe [Types.PatchSource])
gpbrrsSources = Lens.field @"sources"
{-# INLINEABLE gpbrrsSources #-}
{-# DEPRECATED sources "Use generic-lens or generic-optics with 'sources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpbrrsResponseStatus :: Lens.Lens' GetPatchBaselineResponse Core.Int
gpbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gpbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
