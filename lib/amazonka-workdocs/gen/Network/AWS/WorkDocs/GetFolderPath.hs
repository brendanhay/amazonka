{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetFolderPath
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the path information (the hierarchy from the root folder) for the specified folder.
--
-- By default, Amazon WorkDocs returns a maximum of 100 levels upwards from the requested folder and only includes the IDs of the parent folders in the path. You can limit the maximum number of levels. You can also request the parent folder names.
module Network.AWS.WorkDocs.GetFolderPath
  ( -- * Creating a request
    GetFolderPath (..),
    mkGetFolderPath,

    -- ** Request lenses
    gfpFolderId,
    gfpAuthenticationToken,
    gfpFields,
    gfpLimit,
    gfpMarker,

    -- * Destructuring the response
    GetFolderPathResponse (..),
    mkGetFolderPathResponse,

    -- ** Response lenses
    gfprrsPath,
    gfprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetFolderPath' smart constructor.
data GetFolderPath = GetFolderPath'
  { -- | The ID of the folder.
    folderId :: Types.FolderId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationToken,
    -- | A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
    fields :: Core.Maybe Types.Fields,
    -- | The maximum number of levels in the hierarchy to return.
    limit :: Core.Maybe Core.Natural,
    -- | This value is not supported.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolderPath' value with any optional fields omitted.
mkGetFolderPath ::
  -- | 'folderId'
  Types.FolderId ->
  GetFolderPath
mkGetFolderPath folderId =
  GetFolderPath'
    { folderId,
      authenticationToken = Core.Nothing,
      fields = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpFolderId :: Lens.Lens' GetFolderPath Types.FolderId
gfpFolderId = Lens.field @"folderId"
{-# DEPRECATED gfpFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpAuthenticationToken :: Lens.Lens' GetFolderPath (Core.Maybe Types.AuthenticationToken)
gfpAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED gfpAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | A comma-separated list of values. Specify "NAME" to include the names of the parent folders.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpFields :: Lens.Lens' GetFolderPath (Core.Maybe Types.Fields)
gfpFields = Lens.field @"fields"
{-# DEPRECATED gfpFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The maximum number of levels in the hierarchy to return.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpLimit :: Lens.Lens' GetFolderPath (Core.Maybe Core.Natural)
gfpLimit = Lens.field @"limit"
{-# DEPRECATED gfpLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | This value is not supported.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfpMarker :: Lens.Lens' GetFolderPath (Core.Maybe Types.Marker)
gfpMarker = Lens.field @"marker"
{-# DEPRECATED gfpMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.AWSRequest GetFolderPath where
  type Rs GetFolderPath = GetFolderPathResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/folders/" Core.<> (Core.toText folderId)
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
          GetFolderPathResponse'
            Core.<$> (x Core..:? "Path") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFolderPathResponse' smart constructor.
data GetFolderPathResponse = GetFolderPathResponse'
  { -- | The path information.
    path :: Core.Maybe Types.ResourcePath,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolderPathResponse' value with any optional fields omitted.
mkGetFolderPathResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFolderPathResponse
mkGetFolderPathResponse responseStatus =
  GetFolderPathResponse' {path = Core.Nothing, responseStatus}

-- | The path information.
--
-- /Note:/ Consider using 'path' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfprrsPath :: Lens.Lens' GetFolderPathResponse (Core.Maybe Types.ResourcePath)
gfprrsPath = Lens.field @"path"
{-# DEPRECATED gfprrsPath "Use generic-lens or generic-optics with 'path' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfprrsResponseStatus :: Lens.Lens' GetFolderPathResponse Core.Int
gfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
