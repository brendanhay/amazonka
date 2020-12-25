{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the metadata of the specified folder.
module Network.AWS.WorkDocs.GetFolder
  ( -- * Creating a request
    GetFolder (..),
    mkGetFolder,

    -- ** Request lenses
    gfFolderId,
    gfAuthenticationToken,
    gfIncludeCustomMetadata,

    -- * Destructuring the response
    GetFolderResponse (..),
    mkGetFolderResponse,

    -- ** Response lenses
    gfrrsCustomMetadata,
    gfrrsMetadata,
    gfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetFolder' smart constructor.
data GetFolder = GetFolder'
  { -- | The ID of the folder.
    folderId :: Types.ResourceIdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | Set to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFolder' value with any optional fields omitted.
mkGetFolder ::
  -- | 'folderId'
  Types.ResourceIdType ->
  GetFolder
mkGetFolder folderId =
  GetFolder'
    { folderId,
      authenticationToken = Core.Nothing,
      includeCustomMetadata = Core.Nothing
    }

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfFolderId :: Lens.Lens' GetFolder Types.ResourceIdType
gfFolderId = Lens.field @"folderId"
{-# DEPRECATED gfFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfAuthenticationToken :: Lens.Lens' GetFolder (Core.Maybe Types.AuthenticationHeaderType)
gfAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED gfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Set to TRUE to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfIncludeCustomMetadata :: Lens.Lens' GetFolder (Core.Maybe Core.Bool)
gfIncludeCustomMetadata = Lens.field @"includeCustomMetadata"
{-# DEPRECATED gfIncludeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead." #-}

instance Core.AWSRequest GetFolder where
  type Rs GetFolder = GetFolderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath ("/api/v1/folders/" Core.<> (Core.toText folderId)),
        Core._rqQuery =
          Core.toQueryValue "includeCustomMetadata"
            Core.<$> includeCustomMetadata,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFolderResponse'
            Core.<$> (x Core..:? "CustomMetadata")
            Core.<*> (x Core..:? "Metadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFolderResponse' smart constructor.
data GetFolderResponse = GetFolderResponse'
  { -- | The custom metadata on the folder.
    customMetadata :: Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType),
    -- | The metadata of the folder.
    metadata :: Core.Maybe Types.FolderMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetFolderResponse' value with any optional fields omitted.
mkGetFolderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFolderResponse
mkGetFolderResponse responseStatus =
  GetFolderResponse'
    { customMetadata = Core.Nothing,
      metadata = Core.Nothing,
      responseStatus
    }

-- | The custom metadata on the folder.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsCustomMetadata :: Lens.Lens' GetFolderResponse (Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType))
gfrrsCustomMetadata = Lens.field @"customMetadata"
{-# DEPRECATED gfrrsCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | The metadata of the folder.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsMetadata :: Lens.Lens' GetFolderResponse (Core.Maybe Types.FolderMetadata)
gfrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gfrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrrsResponseStatus :: Lens.Lens' GetFolderResponse Core.Int
gfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
