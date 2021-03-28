{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetExport (..)
    , mkGetExport
    -- ** Request lenses
    , geName
    , geVersion
    , geResourceType
    , geExportType

    -- * Destructuring the response
    , GetExportResponse (..)
    , mkGetExportResponse
    -- ** Response lenses
    , gerrsExportStatus
    , gerrsExportType
    , gerrsFailureReason
    , gerrsName
    , gerrsResourceType
    , gerrsUrl
    , gerrsVersion
    , gerrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetExport' smart constructor.
data GetExport = GetExport'
  { name :: Types.Name
    -- ^ The name of the bot to export.
  , version :: Types.NumericalVersion
    -- ^ The version of the bot to export.
  , resourceType :: Types.ResourceType
    -- ^ The type of resource to export. 
  , exportType :: Types.ExportType
    -- ^ The format of the exported data.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExport' value with any optional fields omitted.
mkGetExport
    :: Types.Name -- ^ 'name'
    -> Types.NumericalVersion -- ^ 'version'
    -> Types.ResourceType -- ^ 'resourceType'
    -> Types.ExportType -- ^ 'exportType'
    -> GetExport
mkGetExport name version resourceType exportType
  = GetExport'{name, version, resourceType, exportType}

-- | The name of the bot to export.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geName :: Lens.Lens' GetExport Types.Name
geName = Lens.field @"name"
{-# INLINEABLE geName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The version of the bot to export.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geVersion :: Lens.Lens' GetExport Types.NumericalVersion
geVersion = Lens.field @"version"
{-# INLINEABLE geVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The type of resource to export. 
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geResourceType :: Lens.Lens' GetExport Types.ResourceType
geResourceType = Lens.field @"resourceType"
{-# INLINEABLE geResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The format of the exported data.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
geExportType :: Lens.Lens' GetExport Types.ExportType
geExportType = Lens.field @"exportType"
{-# INLINEABLE geExportType #-}
{-# DEPRECATED exportType "Use generic-lens or generic-optics with 'exportType' instead"  #-}

instance Core.ToQuery GetExport where
        toQuery GetExport{..}
          = Core.toQueryPair "name" name Core.<>
              Core.toQueryPair "version" version
              Core.<> Core.toQueryPair "resourceType" resourceType
              Core.<> Core.toQueryPair "exportType" exportType

instance Core.ToHeaders GetExport where
        toHeaders GetExport{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetExport where
        type Rs GetExport = GetExportResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/exports/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetExportResponse' Core.<$>
                   (x Core..:? "exportStatus") Core.<*> x Core..:? "exportType"
                     Core.<*> x Core..:? "failureReason"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "resourceType"
                     Core.<*> x Core..:? "url"
                     Core.<*> x Core..:? "version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetExportResponse' smart constructor.
data GetExportResponse = GetExportResponse'
  { exportStatus :: Core.Maybe Types.ExportStatus
    -- ^ The status of the export. 
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
  , exportType :: Core.Maybe Types.ExportType
    -- ^ The format of the exported data.
  , failureReason :: Core.Maybe Core.Text
    -- ^ If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
  , name :: Core.Maybe Types.Name
    -- ^ The name of the bot being exported.
  , resourceType :: Core.Maybe Types.ResourceType
    -- ^ The type of the exported resource.
  , url :: Core.Maybe Core.Text
    -- ^ An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
  , version :: Core.Maybe Types.NumericalVersion
    -- ^ The version of the bot being exported.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetExportResponse' value with any optional fields omitted.
mkGetExportResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetExportResponse
mkGetExportResponse responseStatus
  = GetExportResponse'{exportStatus = Core.Nothing,
                       exportType = Core.Nothing, failureReason = Core.Nothing,
                       name = Core.Nothing, resourceType = Core.Nothing,
                       url = Core.Nothing, version = Core.Nothing, responseStatus}

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
{-# INLINEABLE gerrsExportStatus #-}
{-# DEPRECATED exportStatus "Use generic-lens or generic-optics with 'exportStatus' instead"  #-}

-- | The format of the exported data.
--
-- /Note:/ Consider using 'exportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsExportType :: Lens.Lens' GetExportResponse (Core.Maybe Types.ExportType)
gerrsExportType = Lens.field @"exportType"
{-# INLINEABLE gerrsExportType #-}
{-# DEPRECATED exportType "Use generic-lens or generic-optics with 'exportType' instead"  #-}

-- | If @status@ is @FAILED@ , Amazon Lex provides the reason that it failed to export the resource.
--
-- /Note:/ Consider using 'failureReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsFailureReason :: Lens.Lens' GetExportResponse (Core.Maybe Core.Text)
gerrsFailureReason = Lens.field @"failureReason"
{-# INLINEABLE gerrsFailureReason #-}
{-# DEPRECATED failureReason "Use generic-lens or generic-optics with 'failureReason' instead"  #-}

-- | The name of the bot being exported.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsName :: Lens.Lens' GetExportResponse (Core.Maybe Types.Name)
gerrsName = Lens.field @"name"
{-# INLINEABLE gerrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of the exported resource.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResourceType :: Lens.Lens' GetExportResponse (Core.Maybe Types.ResourceType)
gerrsResourceType = Lens.field @"resourceType"
{-# INLINEABLE gerrsResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | An S3 pre-signed URL that provides the location of the exported resource. The exported resource is a ZIP archive that contains the exported resource in JSON format. The structure of the archive may change. Your code should not rely on the archive structure.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsUrl :: Lens.Lens' GetExportResponse (Core.Maybe Core.Text)
gerrsUrl = Lens.field @"url"
{-# INLINEABLE gerrsUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

-- | The version of the bot being exported.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsVersion :: Lens.Lens' GetExportResponse (Core.Maybe Types.NumericalVersion)
gerrsVersion = Lens.field @"version"
{-# INLINEABLE gerrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gerrsResponseStatus :: Lens.Lens' GetExportResponse Core.Int
gerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
