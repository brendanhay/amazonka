{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetImport (..),
    mkGetImport,

    -- ** Request lenses
    giImportId,

    -- * Destructuring the response
    GetImportResponse (..),
    mkGetImportResponse,

    -- ** Response lenses
    girrsCreatedDate,
    girrsFailureReason,
    girrsImportId,
    girrsImportStatus,
    girrsMergeStrategy,
    girrsName,
    girrsResourceType,
    girrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetImport' smart constructor.
newtype GetImport = GetImport'
  { -- | The identifier of the import job information to return.
    importId :: Types.ImportId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetImport' value with any optional fields omitted.
mkGetImport ::
  -- | 'importId'
  Types.ImportId ->
  GetImport
mkGetImport importId = GetImport' {importId}

-- | The identifier of the import job information to return.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giImportId :: Lens.Lens' GetImport Types.ImportId
giImportId = Lens.field @"importId"
{-# DEPRECATED giImportId "Use generic-lens or generic-optics with 'importId' instead." #-}

instance Core.AWSRequest GetImport where
  type Rs GetImport = GetImportResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/imports/" Core.<> (Core.toText importId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetImportResponse'
            Core.<$> (x Core..:? "createdDate")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "importId")
            Core.<*> (x Core..:? "importStatus")
            Core.<*> (x Core..:? "mergeStrategy")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetImportResponse' smart constructor.
data GetImportResponse = GetImportResponse'
  { -- | A timestamp for the date and time that the import job was created.
    createdDate :: Core.Maybe Core.NominalDiffTime,
    -- | A string that describes why an import job failed to complete.
    failureReason :: Core.Maybe [Types.String],
    -- | The identifier for the specific import job.
    importId :: Core.Maybe Types.String,
    -- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
    importStatus :: Core.Maybe Types.ImportStatus,
    -- | The action taken when there was a conflict between an existing resource and a resource in the import file.
    mergeStrategy :: Core.Maybe Types.MergeStrategy,
    -- | The name given to the import job.
    name :: Core.Maybe Types.Name,
    -- | The type of resource imported.
    resourceType :: Core.Maybe Types.ResourceType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetImportResponse' value with any optional fields omitted.
mkGetImportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetImportResponse
mkGetImportResponse responseStatus =
  GetImportResponse'
    { createdDate = Core.Nothing,
      failureReason = Core.Nothing,
      importId = Core.Nothing,
      importStatus = Core.Nothing,
      mergeStrategy = Core.Nothing,
      name = Core.Nothing,
      resourceType = Core.Nothing,
      responseStatus
    }

-- | A timestamp for the date and time that the import job was created.
--
-- /Note:/ Consider using 'createdDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsCreatedDate :: Lens.Lens' GetImportResponse (Core.Maybe Core.NominalDiffTime)
girrsCreatedDate = Lens.field @"createdDate"
{-# DEPRECATED girrsCreatedDate "Use generic-lens or generic-optics with 'createdDate' instead." #-}

-- | A string that describes why an import job failed to complete.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsFailureReason :: Lens.Lens' GetImportResponse (Core.Maybe [Types.String])
girrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED girrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The identifier for the specific import job.
--
-- /Note:/ Consider using 'importId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsImportId :: Lens.Lens' GetImportResponse (Core.Maybe Types.String)
girrsImportId = Lens.field @"importId"
{-# DEPRECATED girrsImportId "Use generic-lens or generic-optics with 'importId' instead." #-}

-- | The status of the import job. If the status is @FAILED@ , you can get the reason for the failure from the @failureReason@ field.
--
-- /Note:/ Consider using 'importStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsImportStatus :: Lens.Lens' GetImportResponse (Core.Maybe Types.ImportStatus)
girrsImportStatus = Lens.field @"importStatus"
{-# DEPRECATED girrsImportStatus "Use generic-lens or generic-optics with 'importStatus' instead." #-}

-- | The action taken when there was a conflict between an existing resource and a resource in the import file.
--
-- /Note:/ Consider using 'mergeStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsMergeStrategy :: Lens.Lens' GetImportResponse (Core.Maybe Types.MergeStrategy)
girrsMergeStrategy = Lens.field @"mergeStrategy"
{-# DEPRECATED girrsMergeStrategy "Use generic-lens or generic-optics with 'mergeStrategy' instead." #-}

-- | The name given to the import job.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsName :: Lens.Lens' GetImportResponse (Core.Maybe Types.Name)
girrsName = Lens.field @"name"
{-# DEPRECATED girrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of resource imported.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResourceType :: Lens.Lens' GetImportResponse (Core.Maybe Types.ResourceType)
girrsResourceType = Lens.field @"resourceType"
{-# DEPRECATED girrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetImportResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
