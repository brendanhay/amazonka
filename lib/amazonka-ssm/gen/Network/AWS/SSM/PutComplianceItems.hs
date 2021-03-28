{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.PutComplianceItems
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a compliance type and other compliance details on a designated resource. This action lets you register custom compliance details with a resource. This call overwrites existing compliance information on the resource, so you must provide a full list of compliance items each time that you send the request.
--
-- ComplianceType can be one of the following:
--
--     * ExecutionId: The execution ID when the patch, association, or custom compliance item was applied.
--
--
--     * ExecutionType: Specify patch, association, or Custom:@string@ .
--
--
--     * ExecutionTime. The time the patch, association, or custom compliance item was applied to the instance.
--
--
--     * Id: The patch, association, or custom compliance ID.
--
--
--     * Title: A title.
--
--
--     * Status: The status of the compliance item. For example, @approved@ for patches, or @Failed@ for associations.
--
--
--     * Severity: A patch severity. For example, @critical@ .
--
--
--     * DocumentName: A SSM document name. For example, AWS-RunPatchBaseline.
--
--
--     * DocumentVersion: An SSM document version number. For example, 4.
--
--
--     * Classification: A patch classification. For example, @security updates@ .
--
--
--     * PatchBaselineId: A patch baseline ID.
--
--
--     * PatchSeverity: A patch severity. For example, @Critical@ .
--
--
--     * PatchState: A patch state. For example, @InstancesWithFailedPatches@ .
--
--
--     * PatchGroup: The name of a patch group.
--
--
--     * InstalledTime: The time the association, patch, or custom compliance item was applied to the resource. Specify the time by using the following format: yyyy-MM-dd'T'HH:mm:ss'Z'
--
--
module Network.AWS.SSM.PutComplianceItems
    (
    -- * Creating a request
      PutComplianceItems (..)
    , mkPutComplianceItems
    -- ** Request lenses
    , pciResourceId
    , pciResourceType
    , pciComplianceType
    , pciExecutionSummary
    , pciItems
    , pciItemContentHash
    , pciUploadType

    -- * Destructuring the response
    , PutComplianceItemsResponse (..)
    , mkPutComplianceItemsResponse
    -- ** Response lenses
    , pcirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkPutComplianceItems' smart constructor.
data PutComplianceItems = PutComplianceItems'
  { resourceId :: Types.ComplianceResourceId
    -- ^ Specify an ID for this resource. For a managed instance, this is the instance ID.
  , resourceType :: Types.ComplianceResourceType
    -- ^ Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
  , complianceType :: Types.ComplianceTypeName
    -- ^ Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
  , executionSummary :: Types.ComplianceExecutionSummary
    -- ^ A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
  , items :: [Types.ComplianceItemEntry]
    -- ^ Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, and so on.
  , itemContentHash :: Core.Maybe Types.ComplianceItemContentHash
    -- ^ MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
  , uploadType :: Core.Maybe Types.ComplianceUploadType
    -- ^ The mode for uploading compliance items. You can specify @COMPLETE@ or @PARTIAL@ . In @COMPLETE@ mode, the system overwrites all existing compliance information for the resource. You must provide a full list of compliance items each time you send the request.
--
-- In @PARTIAL@ mode, the system overwrites compliance information for a specific association. The association must be configured with @SyncCompliance@ set to @MANUAL@ . By default, all requests use @COMPLETE@ mode.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'PutComplianceItems' value with any optional fields omitted.
mkPutComplianceItems
    :: Types.ComplianceResourceId -- ^ 'resourceId'
    -> Types.ComplianceResourceType -- ^ 'resourceType'
    -> Types.ComplianceTypeName -- ^ 'complianceType'
    -> Types.ComplianceExecutionSummary -- ^ 'executionSummary'
    -> PutComplianceItems
mkPutComplianceItems resourceId resourceType complianceType
  executionSummary
  = PutComplianceItems'{resourceId, resourceType, complianceType,
                        executionSummary, items = Core.mempty,
                        itemContentHash = Core.Nothing, uploadType = Core.Nothing}

-- | Specify an ID for this resource. For a managed instance, this is the instance ID.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciResourceId :: Lens.Lens' PutComplianceItems Types.ComplianceResourceId
pciResourceId = Lens.field @"resourceId"
{-# INLINEABLE pciResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Specify the type of resource. @ManagedInstance@ is currently the only supported resource type.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciResourceType :: Lens.Lens' PutComplianceItems Types.ComplianceResourceType
pciResourceType = Lens.field @"resourceType"
{-# INLINEABLE pciResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | Specify the compliance type. For example, specify Association (for a State Manager association), Patch, or Custom:@string@ .
--
-- /Note:/ Consider using 'complianceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciComplianceType :: Lens.Lens' PutComplianceItems Types.ComplianceTypeName
pciComplianceType = Lens.field @"complianceType"
{-# INLINEABLE pciComplianceType #-}
{-# DEPRECATED complianceType "Use generic-lens or generic-optics with 'complianceType' instead"  #-}

-- | A summary of the call execution that includes an execution ID, the type of execution (for example, @Command@ ), and the date/time of the execution using a datetime object that is saved in the following format: yyyy-MM-dd'T'HH:mm:ss'Z'.
--
-- /Note:/ Consider using 'executionSummary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciExecutionSummary :: Lens.Lens' PutComplianceItems Types.ComplianceExecutionSummary
pciExecutionSummary = Lens.field @"executionSummary"
{-# INLINEABLE pciExecutionSummary #-}
{-# DEPRECATED executionSummary "Use generic-lens or generic-optics with 'executionSummary' instead"  #-}

-- | Information about the compliance as defined by the resource type. For example, for a patch compliance type, @Items@ includes information about the PatchSeverity, Classification, and so on.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciItems :: Lens.Lens' PutComplianceItems [Types.ComplianceItemEntry]
pciItems = Lens.field @"items"
{-# INLINEABLE pciItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | MD5 or SHA-256 content hash. The content hash is used to determine if existing information should be overwritten or ignored. If the content hashes match, the request to put compliance information is ignored.
--
-- /Note:/ Consider using 'itemContentHash' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciItemContentHash :: Lens.Lens' PutComplianceItems (Core.Maybe Types.ComplianceItemContentHash)
pciItemContentHash = Lens.field @"itemContentHash"
{-# INLINEABLE pciItemContentHash #-}
{-# DEPRECATED itemContentHash "Use generic-lens or generic-optics with 'itemContentHash' instead"  #-}

-- | The mode for uploading compliance items. You can specify @COMPLETE@ or @PARTIAL@ . In @COMPLETE@ mode, the system overwrites all existing compliance information for the resource. You must provide a full list of compliance items each time you send the request.
--
-- In @PARTIAL@ mode, the system overwrites compliance information for a specific association. The association must be configured with @SyncCompliance@ set to @MANUAL@ . By default, all requests use @COMPLETE@ mode.
--
-- /Note:/ Consider using 'uploadType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pciUploadType :: Lens.Lens' PutComplianceItems (Core.Maybe Types.ComplianceUploadType)
pciUploadType = Lens.field @"uploadType"
{-# INLINEABLE pciUploadType #-}
{-# DEPRECATED uploadType "Use generic-lens or generic-optics with 'uploadType' instead"  #-}

instance Core.ToQuery PutComplianceItems where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders PutComplianceItems where
        toHeaders PutComplianceItems{..}
          = Core.pure ("X-Amz-Target", "AmazonSSM.PutComplianceItems")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON PutComplianceItems where
        toJSON PutComplianceItems{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceId" Core..= resourceId),
                  Core.Just ("ResourceType" Core..= resourceType),
                  Core.Just ("ComplianceType" Core..= complianceType),
                  Core.Just ("ExecutionSummary" Core..= executionSummary),
                  Core.Just ("Items" Core..= items),
                  ("ItemContentHash" Core..=) Core.<$> itemContentHash,
                  ("UploadType" Core..=) Core.<$> uploadType])

instance Core.AWSRequest PutComplianceItems where
        type Rs PutComplianceItems = PutComplianceItemsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 PutComplianceItemsResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkPutComplianceItemsResponse' smart constructor.
newtype PutComplianceItemsResponse = PutComplianceItemsResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PutComplianceItemsResponse' value with any optional fields omitted.
mkPutComplianceItemsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> PutComplianceItemsResponse
mkPutComplianceItemsResponse responseStatus
  = PutComplianceItemsResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcirrsResponseStatus :: Lens.Lens' PutComplianceItemsResponse Core.Int
pcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE pcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
