{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetDocumentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves version metadata for the specified document.
module Network.AWS.WorkDocs.GetDocumentVersion
  ( -- * Creating a request
    GetDocumentVersion (..),
    mkGetDocumentVersion,

    -- ** Request lenses
    gdvDocumentId,
    gdvVersionId,
    gdvAuthenticationToken,
    gdvFields,
    gdvIncludeCustomMetadata,

    -- * Destructuring the response
    GetDocumentVersionResponse (..),
    mkGetDocumentVersionResponse,

    -- ** Response lenses
    gdvrrsCustomMetadata,
    gdvrrsMetadata,
    gdvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | The version ID of the document.
    versionId :: Types.VersionId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
    fields :: Core.Maybe Types.Fields,
    -- | Set this to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentVersion' value with any optional fields omitted.
mkGetDocumentVersion ::
  -- | 'documentId'
  Types.DocumentId ->
  -- | 'versionId'
  Types.VersionId ->
  GetDocumentVersion
mkGetDocumentVersion documentId versionId =
  GetDocumentVersion'
    { documentId,
      versionId,
      authenticationToken = Core.Nothing,
      fields = Core.Nothing,
      includeCustomMetadata = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvDocumentId :: Lens.Lens' GetDocumentVersion Types.DocumentId
gdvDocumentId = Lens.field @"documentId"
{-# DEPRECATED gdvDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | The version ID of the document.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvVersionId :: Lens.Lens' GetDocumentVersion Types.VersionId
gdvVersionId = Lens.field @"versionId"
{-# DEPRECATED gdvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvAuthenticationToken :: Lens.Lens' GetDocumentVersion (Core.Maybe Types.AuthenticationHeaderType)
gdvAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED gdvAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvFields :: Lens.Lens' GetDocumentVersion (Core.Maybe Types.Fields)
gdvFields = Lens.field @"fields"
{-# DEPRECATED gdvFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | Set this to TRUE to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvIncludeCustomMetadata :: Lens.Lens' GetDocumentVersion (Core.Maybe Core.Bool)
gdvIncludeCustomMetadata = Lens.field @"includeCustomMetadata"
{-# DEPRECATED gdvIncludeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead." #-}

instance Core.AWSRequest GetDocumentVersion where
  type Rs GetDocumentVersion = GetDocumentVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/documents/" Core.<> (Core.toText documentId)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionId)
            ),
        Core._rqQuery =
          Core.toQueryValue "fields" Core.<$> fields
            Core.<> ( Core.toQueryValue "includeCustomMetadata"
                        Core.<$> includeCustomMetadata
                    ),
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentVersionResponse'
            Core.<$> (x Core..:? "CustomMetadata")
            Core.<*> (x Core..:? "Metadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { -- | The custom metadata on the document version.
    customMetadata :: Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType),
    -- | The version metadata.
    metadata :: Core.Maybe Types.DocumentVersionMetadata,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetDocumentVersionResponse' value with any optional fields omitted.
mkGetDocumentVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDocumentVersionResponse
mkGetDocumentVersionResponse responseStatus =
  GetDocumentVersionResponse'
    { customMetadata = Core.Nothing,
      metadata = Core.Nothing,
      responseStatus
    }

-- | The custom metadata on the document version.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsCustomMetadata :: Lens.Lens' GetDocumentVersionResponse (Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType))
gdvrrsCustomMetadata = Lens.field @"customMetadata"
{-# DEPRECATED gdvrrsCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | The version metadata.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsMetadata :: Lens.Lens' GetDocumentVersionResponse (Core.Maybe Types.DocumentVersionMetadata)
gdvrrsMetadata = Lens.field @"metadata"
{-# DEPRECATED gdvrrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsResponseStatus :: Lens.Lens' GetDocumentVersionResponse Core.Int
gdvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
