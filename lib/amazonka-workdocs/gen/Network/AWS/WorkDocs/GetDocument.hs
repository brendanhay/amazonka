{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a document.
module Network.AWS.WorkDocs.GetDocument
  ( -- * Creating a request
    GetDocument (..),
    mkGetDocument,

    -- ** Request lenses
    gdDocumentId,
    gdAuthenticationToken,
    gdIncludeCustomMetadata,

    -- * Destructuring the response
    GetDocumentResponse (..),
    mkGetDocumentResponse,

    -- ** Response lenses
    gdrrsCustomMetadata,
    gdrrsMetadata,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | Set this to @TRUE@ to include custom metadata in the response.
    includeCustomMetadata :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocument' value with any optional fields omitted.
mkGetDocument ::
  -- | 'documentId'
  Types.DocumentId ->
  GetDocument
mkGetDocument documentId =
  GetDocument'
    { documentId,
      authenticationToken = Core.Nothing,
      includeCustomMetadata = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentId :: Lens.Lens' GetDocument Types.DocumentId
gdDocumentId = Lens.field @"documentId"
{-# DEPRECATED gdDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAuthenticationToken :: Lens.Lens' GetDocument (Core.Maybe Types.AuthenticationHeaderType)
gdAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED gdAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Set this to @TRUE@ to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdIncludeCustomMetadata :: Lens.Lens' GetDocument (Core.Maybe Core.Bool)
gdIncludeCustomMetadata = Lens.field @"includeCustomMetadata"
{-# DEPRECATED gdIncludeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead." #-}

instance Core.AWSRequest GetDocument where
  type Rs GetDocument = GetDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/api/v1/documents/" Core.<> (Core.toText documentId)),
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
          GetDocumentResponse'
            Core.<$> (x Core..:? "CustomMetadata")
            Core.<*> (x Core..:? "Metadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | The custom metadata on the document.
    customMetadata :: Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType),
    -- | The metadata details of the document.
    metadata :: Core.Maybe Types.DocumentMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDocumentResponse' value with any optional fields omitted.
mkGetDocumentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDocumentResponse
mkGetDocumentResponse responseStatus =
  GetDocumentResponse'
    { customMetadata = Core.Nothing,
      metadata = Core.Nothing,
      responseStatus
    }

-- | The custom metadata on the document.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsCustomMetadata :: Lens.Lens' GetDocumentResponse (Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType))
gdrrsCustomMetadata = Lens.field @"customMetadata"
{-# DEPRECATED gdrrsCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | The metadata details of the document.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsMetadata :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentMetadata)
gdrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gdrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDocumentResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
