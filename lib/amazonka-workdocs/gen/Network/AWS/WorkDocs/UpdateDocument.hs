{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.
module Network.AWS.WorkDocs.UpdateDocument
  ( -- * Creating a request
    UpdateDocument (..),
    mkUpdateDocument,

    -- ** Request lenses
    udDocumentId,
    udAuthenticationToken,
    udName,
    udParentFolderId,
    udResourceState,

    -- * Destructuring the response
    UpdateDocumentResponse (..),
    mkUpdateDocumentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The name of the document.
    name :: Core.Maybe Types.Name,
    -- | The ID of the parent folder.
    parentFolderId :: Core.Maybe Types.ParentFolderId,
    -- | The resource state of the document. Only ACTIVE and RECYCLED are supported.
    resourceState :: Core.Maybe Types.ResourceStateType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocument' value with any optional fields omitted.
mkUpdateDocument ::
  -- | 'documentId'
  Types.DocumentId ->
  UpdateDocument
mkUpdateDocument documentId =
  UpdateDocument'
    { documentId,
      authenticationToken = Core.Nothing,
      name = Core.Nothing,
      parentFolderId = Core.Nothing,
      resourceState = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocumentId :: Lens.Lens' UpdateDocument Types.DocumentId
udDocumentId = Lens.field @"documentId"
{-# DEPRECATED udDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udAuthenticationToken :: Lens.Lens' UpdateDocument (Core.Maybe Types.AuthenticationHeaderType)
udAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED udAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDocument (Core.Maybe Types.Name)
udName = Lens.field @"name"
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udParentFolderId :: Lens.Lens' UpdateDocument (Core.Maybe Types.ParentFolderId)
udParentFolderId = Lens.field @"parentFolderId"
{-# DEPRECATED udParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | The resource state of the document. Only ACTIVE and RECYCLED are supported.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udResourceState :: Lens.Lens' UpdateDocument (Core.Maybe Types.ResourceStateType)
udResourceState = Lens.field @"resourceState"
{-# DEPRECATED udResourceState "Use generic-lens or generic-optics with 'resourceState' instead." #-}

instance Core.FromJSON UpdateDocument where
  toJSON UpdateDocument {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("ParentFolderId" Core..=) Core.<$> parentFolderId,
            ("ResourceState" Core..=) Core.<$> resourceState
          ]
      )

instance Core.AWSRequest UpdateDocument where
  type Rs UpdateDocument = UpdateDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/api/v1/documents/" Core.<> (Core.toText documentId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateDocumentResponse'

-- | /See:/ 'mkUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentResponse' value with any optional fields omitted.
mkUpdateDocumentResponse ::
  UpdateDocumentResponse
mkUpdateDocumentResponse = UpdateDocumentResponse'
