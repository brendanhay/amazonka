{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteFolder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified folder and its contents.
module Network.AWS.WorkDocs.DeleteFolder
  ( -- * Creating a request
    DeleteFolder (..),
    mkDeleteFolder,

    -- ** Request lenses
    dfFolderId,
    dfAuthenticationToken,

    -- * Destructuring the response
    DeleteFolderResponse (..),
    mkDeleteFolderResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteFolder' smart constructor.
data DeleteFolder = DeleteFolder'
  { -- | The ID of the folder.
    folderId :: Types.ResourceIdType,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFolder' value with any optional fields omitted.
mkDeleteFolder ::
  -- | 'folderId'
  Types.ResourceIdType ->
  DeleteFolder
mkDeleteFolder folderId =
  DeleteFolder' {folderId, authenticationToken = Core.Nothing}

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFolderId :: Lens.Lens' DeleteFolder Types.ResourceIdType
dfFolderId = Lens.field @"folderId"
{-# DEPRECATED dfFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfAuthenticationToken :: Lens.Lens' DeleteFolder (Core.Maybe Types.AuthenticationHeaderType)
dfAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dfAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest DeleteFolder where
  type Rs DeleteFolder = DeleteFolderResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath ("/api/v1/folders/" Core.<> (Core.toText folderId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteFolderResponse'

-- | /See:/ 'mkDeleteFolderResponse' smart constructor.
data DeleteFolderResponse = DeleteFolderResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFolderResponse' value with any optional fields omitted.
mkDeleteFolderResponse ::
  DeleteFolderResponse
mkDeleteFolderResponse = DeleteFolderResponse'
