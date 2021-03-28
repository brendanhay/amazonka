{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDocumentVersion (..)
    , mkGetDocumentVersion
    -- ** Request lenses
    , gdvDocumentId
    , gdvVersionId
    , gdvAuthenticationToken
    , gdvFields
    , gdvIncludeCustomMetadata

    -- * Destructuring the response
    , GetDocumentVersionResponse (..)
    , mkGetDocumentVersionResponse
    -- ** Response lenses
    , gdvrrsCustomMetadata
    , gdvrrsMetadata
    , gdvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { documentId :: Types.DocumentId
    -- ^ The ID of the document.
  , versionId :: Types.VersionId
    -- ^ The version ID of the document.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , fields :: Core.Maybe Types.Fields
    -- ^ A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
  , includeCustomMetadata :: Core.Maybe Core.Bool
    -- ^ Set this to TRUE to include custom metadata in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentVersion' value with any optional fields omitted.
mkGetDocumentVersion
    :: Types.DocumentId -- ^ 'documentId'
    -> Types.VersionId -- ^ 'versionId'
    -> GetDocumentVersion
mkGetDocumentVersion documentId versionId
  = GetDocumentVersion'{documentId, versionId,
                        authenticationToken = Core.Nothing, fields = Core.Nothing,
                        includeCustomMetadata = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvDocumentId :: Lens.Lens' GetDocumentVersion Types.DocumentId
gdvDocumentId = Lens.field @"documentId"
{-# INLINEABLE gdvDocumentId #-}
{-# DEPRECATED documentId "Use generic-lens or generic-optics with 'documentId' instead"  #-}

-- | The version ID of the document.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvVersionId :: Lens.Lens' GetDocumentVersion Types.VersionId
gdvVersionId = Lens.field @"versionId"
{-# INLINEABLE gdvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvAuthenticationToken :: Lens.Lens' GetDocumentVersion (Core.Maybe Types.AuthenticationHeaderType)
gdvAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE gdvAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvFields :: Lens.Lens' GetDocumentVersion (Core.Maybe Types.Fields)
gdvFields = Lens.field @"fields"
{-# INLINEABLE gdvFields #-}
{-# DEPRECATED fields "Use generic-lens or generic-optics with 'fields' instead"  #-}

-- | Set this to TRUE to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvIncludeCustomMetadata :: Lens.Lens' GetDocumentVersion (Core.Maybe Core.Bool)
gdvIncludeCustomMetadata = Lens.field @"includeCustomMetadata"
{-# INLINEABLE gdvIncludeCustomMetadata #-}
{-# DEPRECATED includeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead"  #-}

instance Core.ToQuery GetDocumentVersion where
        toQuery GetDocumentVersion{..}
          = Core.maybe Core.mempty (Core.toQueryPair "fields") fields Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "includeCustomMetadata")
                includeCustomMetadata

instance Core.ToHeaders GetDocumentVersion where
        toHeaders GetDocumentVersion{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetDocumentVersion where
        type Rs GetDocumentVersion = GetDocumentVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/api/v1/documents/" Core.<> Core.toText documentId Core.<>
                             "/versions/"
                             Core.<> Core.toText versionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDocumentVersionResponse' Core.<$>
                   (x Core..:? "CustomMetadata") Core.<*> x Core..:? "Metadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { customMetadata :: Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType)
    -- ^ The custom metadata on the document version.
  , metadata :: Core.Maybe Types.DocumentVersionMetadata
    -- ^ The version metadata.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDocumentVersionResponse' value with any optional fields omitted.
mkGetDocumentVersionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDocumentVersionResponse
mkGetDocumentVersionResponse responseStatus
  = GetDocumentVersionResponse'{customMetadata = Core.Nothing,
                                metadata = Core.Nothing, responseStatus}

-- | The custom metadata on the document version.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsCustomMetadata :: Lens.Lens' GetDocumentVersionResponse (Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType))
gdvrrsCustomMetadata = Lens.field @"customMetadata"
{-# INLINEABLE gdvrrsCustomMetadata #-}
{-# DEPRECATED customMetadata "Use generic-lens or generic-optics with 'customMetadata' instead"  #-}

-- | The version metadata.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsMetadata :: Lens.Lens' GetDocumentVersionResponse (Core.Maybe Types.DocumentVersionMetadata)
gdvrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE gdvrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrrsResponseStatus :: Lens.Lens' GetDocumentVersionResponse Core.Int
gdvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
