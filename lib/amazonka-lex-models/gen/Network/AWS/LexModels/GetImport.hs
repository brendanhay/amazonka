{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetImport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about an import job started with the @StartImport@ operation.
module Network.AWS.LexModels.GetImport
    (
    -- * Creating a request
      GetImport (..)
    , mkGetImport
    -- ** Request lenses
    , giImportId

    -- * Destructuring the response
    , GetImportResponse (..)
    , mkGetImportResponse
    -- ** Response lenses
    , girrsCreatedDate
    , girrsFailureReason
    , girrsImportId
    , girrsImportStatus
    , girrsMergeStrategy
    , girrsName
    , girrsResourceType
    , girrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetImport' smart constructor.
newtype GetImport = GetImport'
  { importId :: Core.Text
    -- ^ The identifier of the import job information to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetImport' value with any optional fields omitted.
mkGetImport
    :: Core.Text -- ^ 'importId'
    -> GetImport
mkGetImport importId = GetImport'{importId}

-- | The identifier of the import job information to return.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giImportId :: Lens.Lens' GetImport Core.Text
giImportId = Lens.field @"importId"
{-# INLINEABLE giImportId #-}
{-# DEPRECATED importId "Use generic-lens or generic-optics with 'importId' instead"  #-}

instance Core.ToQuery GetImport where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetImport where
        toHeaders GetImport{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetImport where
        type Rs GetImport = GetImportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/imports/" Core.<> Core.toText importId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetImportResponse' Core.<$>
                   (x Core..:? "createdDate") Core.<*> x Core..:? "failureReason"
                     Core.<*> x Core..:? "importId"
                     Core.<*> x Core..:? "importStatus"
                     Core.<*> x Core..:? "mergeStrategy"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "resourceType"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { createdDate :: Core.Maybe Core.NominalDiffTime
    -- ^ A timestamp for the date and time that the import job was created.
  , failureReason :: Core.Maybe [Core.Text]
    -- ^ A string that describes why an import job failed to complete.
  , importId :: Core.Maybe Core.Text
    -- ^ The identifier for the specific import job.
  , importStatus :: Core.Maybe Types.ImportStatus
    -- ^ The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
  , mergeStrategy :: Core.Maybe Types.MergeStrategy
    -- ^ The action taken when there was a conflict between an existing resource and a resource in the import file.
  , name :: Core.Maybe Types.Name
    -- ^ The name given to the import job.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of resource imported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetImportResponse' value with any optional fields omitted.
mkGetImportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetImportResponse
mkGetImportResponse responseStatus
  = GetImportResponse'{createdDate = Core.Nothing,
                       failureReason = Core.Nothing, importId = Core.Nothing,
                       importStatus = Core.Nothing, mergeStrategy = Core.Nothing,
                       name = Core.Nothing, resourceType = Core.Nothing, responseStatus}

-- | A timestamp for the date and time that the import job was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsCreatedDate :: Lens.Lens' GetImportResponse (Core.Maybe Core.NominalDiffTime)
girrsCreatedDate = Lens.field @"createdDate"
{-# INLINEABLE girrsCreatedDate #-}
{-# DEPRECATED createdDate "Use generic-lens or generic-optics with 'createdDate' instead"  #-}

-- | A string that describes why an import job failed to complete.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsFailureReason :: Lens.Lens' GetImportResponse (Core.Maybe [Core.Text])
girrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE girrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The identifier for the specific import job.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsImportId :: Lens.Lens' GetImportResponse (Core.Maybe Core.Text)
girrsImportId = Lens.field @"importId"
{-# INLINEABLE girrsImportId #-}
{-# DEPRECATED importId "Use generic-lens or generic-optics with 'importId' instead"  #-}

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsImportStatus :: Lens.Lens' GetImportResponse (Core.Maybe Types.ImportStatus)
girrsImportStatus = Lens.field @"importStatus"
{-# INLINEABLE girrsImportStatus #-}
{-# DEPRECATED importStatus "Use generic-lens or generic-optics with 'importStatus' instead"  #-}

-- | The action taken when there was a conflict between an existing resource and a resource in the import file.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsMergeStrategy :: Lens.Lens' GetImportResponse (Core.Maybe Types.MergeStrategy)
girrsMergeStrategy = Lens.field @"mergeStrategy"
{-# INLINEABLE girrsMergeStrategy #-}
{-# DEPRECATED mergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead"  #-}

-- | The name given to the import job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsName :: Lens.Lens' GetImportResponse (Core.Maybe Types.Name)
girrsName = Lens.field @"name"
{-# INLINEABLE girrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of resource imported.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResourceType :: Lens.Lens' GetImportResponse (Core.Maybe Types.ResourceType)
girrsResourceType = Lens.field @"resourceType"
{-# INLINEABLE girrsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetImportResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE girrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
