{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.GetExport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports the contents of a Amazon Lex resource in a specified format.
module Network.AWS.LexModels.GetExport
  ( -- * Creating a request
    GetExport (..),
    mkGetExport,

    -- ** Request lenses
    geName,
    geVersion,
    geResourceType,
    geExportType,

    -- * Destructuring the response
    GetExportResponse (..),
    mkGetExportResponse,

    -- ** Response lenses
    gerrsExportStatus,
    gerrsExportType,
    gerrsFailureReason,
    gerrsName,
    gerrsResourceType,
    gerrsUrl,
    gerrsVersion,
    gerrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExport' smart constructor.
data GetExport = GetExport'
  { -- | The name of the bot to export.
    name :: Types.Name,
    -- | The version of the bot to export.
    version :: Types.NumericalVersion,
    -- | The type of resource to export.
    resourceType :: Types.ResourceType,
    -- | The format of the exported data.
    exportType :: Types.ExportType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExport' value with any optional fields omitted.
mkGetExport ::
  -- | 'name'
  Types.Name ->
  -- | 'version'
  Types.NumericalVersion ->
  -- | 'resourceType'
  Types.ResourceType ->
  -- | 'exportType'
  Types.ExportType ->
  GetExport
mkGetExport name version resourceType exportType =
  GetExport' {name, version, resourceType, exportType}

-- | The name of the bot to export.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geName :: Lens.Lens' GetExport Types.Name
geName = Lens.field @"name"
{-# DEPRECATED geName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the bot to export.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geVersion :: Lens.Lens' GetExport Types.NumericalVersion
geVersion = Lens.field @"version"
{-# DEPRECATED geVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The type of resource to export.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geResourceType :: Lens.Lens' GetExport Types.ResourceType
geResourceType = Lens.field @"resourceType"
{-# DEPRECATED geResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | The format of the exported data.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geExportType :: Lens.Lens' GetExport Types.ExportType
geExportType = Lens.field @"exportType"
{-# DEPRECATED geExportType "Use generic-lens or generic-optics with 'exportType' instead." #-}

instance Core.AWSRequest GetExport where
  type Rs GetExport = GetExportResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/exports/",
        Core._rqQuery =
          Core.toQueryValue "name" name
            Core.<> (Core.toQueryValue "version" version)
            Core.<> (Core.toQueryValue "resourceType" resourceType)
            Core.<> (Core.toQueryValue "exportType" exportType),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportResponse'
            Core.<$> (x Core..:? "exportStatus")
            Core.<*> (x Core..:? "exportType")
            Core.<*> (x Core..:? "failureReason")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "resourceType")
            Core.<*> (x Core..:? "url")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { -- | The status of the export.
    --
    --
    --     * @IN_PROGRESS@ - The export is in progress.
    --
    --
    --     * @READY@ - The export is complete.
    --
    --
    --     * @FAILED@ - The export could not be completed.
    exportStatus :: Core.Maybe Types.ExportStatus,
    -- | The format of the exported data.
    exportType :: Core.Maybe Types.ExportType,
    -- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
    failureReason :: Core.Maybe Types.String,
    -- | The name of the bot being exported.
    name :: Core.Maybe Types.Name,
    -- | The type of the exported resource.
    resourceType :: Core.Maybe Types.ResourceType,
    -- | An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
    url :: Core.Maybe Types.String,
    -- | The version of the bot being exported.
    version :: Core.Maybe Types.NumericalVersion,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportResponse' value with any optional fields omitted.
mkGetExportResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetExportResponse
mkGetExportResponse responseStatus =
  GetExportResponse'
    { exportStatus = Core.Nothing,
      exportType = Core.Nothing,
      failureReason = Core.Nothing,
      name = Core.Nothing,
      resourceType = Core.Nothing,
      url = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The status of the export.
--
--
--     * @IN_PROGRESS@ - The export is in progress.
--
--
--     * @READY@ - The export is complete.
--
--
--     * @FAILED@ - The export could not be completed.
--
--
--
-- /Note:/ Consider using 'exportStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsExportStatus :: Lens.Lens' GetExportResponse (Core.Maybe Types.ExportStatus)
gerrsExportStatus = Lens.field @"exportStatus"
{-# DEPRECATED gerrsExportStatus "Use generic-lens or generic-optics with 'exportStatus' instead." #-}

-- | The format of the exported data.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsExportType :: Lens.Lens' GetExportResponse (Core.Maybe Types.ExportType)
gerrsExportType = Lens.field @"exportType"
{-# DEPRECATED gerrsExportType "Use generic-lens or generic-optics with 'exportType' instead." #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsFailureReason :: Lens.Lens' GetExportResponse (Core.Maybe Types.String)
gerrsFailureReason = Lens.field @"failureReason"
{-# DEPRECATED gerrsFailureReason "Use generic-lens or generic-optics with 'failureReason' instead." #-}

-- | The name of the bot being exported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsName :: Lens.Lens' GetExportResponse (Core.Maybe Types.Name)
gerrsName = Lens.field @"name"
{-# DEPRECATED gerrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the exported resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResourceType :: Lens.Lens' GetExportResponse (Core.Maybe Types.ResourceType)
gerrsResourceType = Lens.field @"resourceType"
{-# DEPRECATED gerrsResourceType "Use generic-lens or generic-optics with 'resourceType' instead." #-}

-- | An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsUrl :: Lens.Lens' GetExportResponse (Core.Maybe Types.String)
gerrsUrl = Lens.field @"url"
{-# DEPRECATED gerrsUrl "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The version of the bot being exported.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsVersion :: Lens.Lens' GetExportResponse (Core.Maybe Types.NumericalVersion)
gerrsVersion = Lens.field @"version"
{-# DEPRECATED gerrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResponseStatus :: Lens.Lens' GetExportResponse Core.Int
gerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
