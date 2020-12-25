{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetDocumentPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the requested document.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested document and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the names of the parent folders.
module Network.AWS.WorkDocs.GetDocumentPath
  ( -- * Creating a request
    GetDocumentPath (..),
    mkGetDocumentPath,

    -- ** Request lenses
    gdpDocumentId,
    gdpAuthenticationToken,
    gdpFields,
    gdpLimit,
    gdpMarker,

    -- * Destructuring the response
    GetDocumentPathResponse (..),
    mkGetDocumentPathResponse,

    -- ** Response lenses
    gdprrsPath,
    gdprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetDocumentPath' smart constructor.
data GetDocumentPath = GetDocumentPath'
  { -- | The ID of the document.
    documentId :: Types.IdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | A comma-separated list of values. Specify @NAME@ to include the names of the parent folders.
    fields :: Core.Maybe Types.Fields,
    -- | The maximum number of levels in the hierarchy to return.
    limit :: Core.Maybe Core.Natural,
    -- | This value is not supported.
    marker :: Core.Maybe Types.PageMarkerType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentPath' value with any optional fields omitted.
mkGetDocumentPath ::
  -- | 'documentId'
  Types.IdType ->
  GetDocumentPath
mkGetDocumentPath documentId =
  GetDocumentPath'
    { documentId,
      authenticationToken = Core.Nothing,
      fields = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpDocumentId :: Lens.Lens' GetDocumentPath Types.IdType
gdpDocumentId = Lens.field @"documentId"
{-# DEPRECATED gdpDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpAuthenticationToken :: Lens.Lens' GetDocumentPath (Core.Maybe Types.AuthenticationHeaderType)
gdpAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED gdpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | A comma-separated list of values. Specify @NAME@ to include the names of the parent folders.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpFields :: Lens.Lens' GetDocumentPath (Core.Maybe Types.Fields)
gdpFields = Lens.field @"fields"
{-# DEPRECATED gdpFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The maximum number of levels in the hierarchy to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpLimit :: Lens.Lens' GetDocumentPath (Core.Maybe Core.Natural)
gdpLimit = Lens.field @"limit"
{-# DEPRECATED gdpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | This value is not supported.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdpMarker :: Lens.Lens' GetDocumentPath (Core.Maybe Types.PageMarkerType)
gdpMarker = Lens.field @"marker"
{-# DEPRECATED gdpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.AWSRequest GetDocumentPath where
  type Rs GetDocumentPath = GetDocumentPathResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/documents/" Core.<> (Core.toText documentId)
                Core.<> ("/path")
            ),
        Core._rqQuery =
          Core.toQueryValue "fields" Core.<$> fields
            Core.<> (Core.toQueryValue "limit" Core.<$> limit)
            Core.<> (Core.toQueryValue "marker" Core.<$> marker),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentPathResponse'
            Core.<$> (x Core..:? "Path") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDocumentPathResponse' smart constructor.
data GetDocumentPathResponse = GetDocumentPathResponse'
  { -- | The path information.
    path :: Core.Maybe Types.ResourcePath,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentPathResponse' value with any optional fields omitted.
mkGetDocumentPathResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDocumentPathResponse
mkGetDocumentPathResponse responseStatus =
  GetDocumentPathResponse' {path = Core.Nothing, responseStatus}

-- | The path information.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsPath :: Lens.Lens' GetDocumentPathResponse (Core.Maybe Types.ResourcePath)
gdprrsPath = Lens.field @"path"
{-# DEPRECATED gdprrsPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdprrsResponseStatus :: Lens.Lens' GetDocumentPathResponse Core.Int
gdprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
