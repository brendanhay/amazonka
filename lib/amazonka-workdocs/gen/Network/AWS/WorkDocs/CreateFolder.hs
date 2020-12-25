{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.CreateFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a folder with the specified name and parent folder.
module Network.AWS.WorkDocs.CreateFolder
  ( -- * Creating a request
    CreateFolder (..),
    mkCreateFolder,

    -- ** Request lenses
    cfParentFolderId,
    cfAuthenticationToken,
    cfName,

    -- * Destructuring the response
    CreateFolderResponse (..),
    mkCreateFolderResponse,

    -- ** Response lenses
    cfrrsMetadata,
    cfrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkCreateFolder' smart constructor.
data CreateFolder = CreateFolder'
  { -- | The ID of the parent folder.
    parentFolderId :: Types.ResourceIdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The name of the new folder.
    name :: Core.Maybe Types.ResourceNameType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateFolder' value with any optional fields omitted.
mkCreateFolder ::
  -- | 'parentFolderId'
  Types.ResourceIdType ->
  CreateFolder
mkCreateFolder parentFolderId =
  CreateFolder'
    { parentFolderId,
      authenticationToken = Core.Nothing,
      name = Core.Nothing
    }

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfParentFolderId :: Lens.Lens' CreateFolder Types.ResourceIdType
cfParentFolderId = Lens.field @"parentFolderId"
{-# DEPRECATED cfParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfAuthenticationToken :: Lens.Lens' CreateFolder (Core.Maybe Types.AuthenticationHeaderType)
cfAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED cfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The name of the new folder.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfName :: Lens.Lens' CreateFolder (Core.Maybe Types.ResourceNameType)
cfName = Lens.field @"name"
{-# DEPRECATED cfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON CreateFolder where
  toJSON CreateFolder {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ParentFolderId" Core..= parentFolderId),
            ("Name" Core..=) Core.<$> name
          ]
      )

instance Core.AWSRequest CreateFolder where
  type Rs CreateFolder = CreateFolderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/api/v1/folders",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateFolderResponse'
            Core.<$> (x Core..:? "Metadata") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateFolderResponse' smart constructor.
data CreateFolderResponse = CreateFolderResponse'
  { -- | The metadata of the folder.
    metadata :: Core.Maybe Types.FolderMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateFolderResponse' value with any optional fields omitted.
mkCreateFolderResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateFolderResponse
mkCreateFolderResponse responseStatus =
  CreateFolderResponse' {metadata = Core.Nothing, responseStatus}

-- | The metadata of the folder.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsMetadata :: Lens.Lens' CreateFolderResponse (Core.Maybe Types.FolderMetadata)
cfrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED cfrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfrrsResponseStatus :: Lens.Lens' CreateFolderResponse Core.Int
cfrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cfrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
