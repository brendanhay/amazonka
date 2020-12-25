{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified document and its associated metadata.
module Network.AWS.WorkDocs.DeleteDocument
  ( -- * Creating a request
    DeleteDocument (..),
    mkDeleteDocument,

    -- ** Request lenses
    ddDocumentId,
    ddAuthenticationToken,

    -- * Destructuring the response
    DeleteDocumentResponse (..),
    mkDeleteDocumentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocument' value with any optional fields omitted.
mkDeleteDocument ::
  -- | 'documentId'
  Types.DocumentId ->
  DeleteDocument
mkDeleteDocument documentId =
  DeleteDocument' {documentId, authenticationToken = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentId :: Lens.Lens' DeleteDocument Types.DocumentId
ddDocumentId = Lens.field @"documentId"
{-# DEPRECATED ddDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAuthenticationToken :: Lens.Lens' DeleteDocument (Core.Maybe Types.AuthenticationHeaderType)
ddAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED ddAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ("/api/v1/documents/" Core.<> (Core.toText documentId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response = Response.receiveNull DeleteDocumentResponse'

-- | /See:/ 'mkDeleteDocumentResponse' smart constructor.
data DeleteDocumentResponse = DeleteDocumentResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentResponse' value with any optional fields omitted.
mkDeleteDocumentResponse ::
  DeleteDocumentResponse
mkDeleteDocumentResponse = DeleteDocumentResponse'
