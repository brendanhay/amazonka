{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteFolderContents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the contents of the specified folder.
module Network.AWS.WorkDocs.DeleteFolderContents
  ( -- * Creating a request
    DeleteFolderContents (..),
    mkDeleteFolderContents,

    -- ** Request lenses
    dfcFolderId,
    dfcAuthenticationToken,

    -- * Destructuring the response
    DeleteFolderContentsResponse (..),
    mkDeleteFolderContentsResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteFolderContents' smart constructor.
data DeleteFolderContents = DeleteFolderContents'
  { -- | The ID of the folder.
    folderId :: Types.FolderId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFolderContents' value with any optional fields omitted.
mkDeleteFolderContents ::
  -- | 'folderId'
  Types.FolderId ->
  DeleteFolderContents
mkDeleteFolderContents folderId =
  DeleteFolderContents'
    { folderId,
      authenticationToken = Core.Nothing
    }

-- | The ID of the folder.
--
-- /Note:/ Consider using 'folderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcFolderId :: Lens.Lens' DeleteFolderContents Types.FolderId
dfcFolderId = Lens.field @"folderId"
{-# DEPRECATED dfcFolderId "Use generic-lens or generic-optics with 'folderId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfcAuthenticationToken :: Lens.Lens' DeleteFolderContents (Core.Maybe Types.AuthenticationToken)
dfcAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED dfcAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest DeleteFolderContents where
  type Rs DeleteFolderContents = DeleteFolderContentsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/folders/" Core.<> (Core.toText folderId)
                Core.<> ("/contents")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteFolderContentsResponse'

-- | /See:/ 'mkDeleteFolderContentsResponse' smart constructor.
data DeleteFolderContentsResponse = DeleteFolderContentsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteFolderContentsResponse' value with any optional fields omitted.
mkDeleteFolderContentsResponse ::
  DeleteFolderContentsResponse
mkDeleteFolderContentsResponse = DeleteFolderContentsResponse'
