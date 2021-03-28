{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetDocument (..)
    , mkGetDocument
    -- ** Request lenses
    , gdDocumentId
    , gdAuthenticationToken
    , gdIncludeCustomMetadata

    -- * Destructuring the response
    , GetDocumentResponse (..)
    , mkGetDocumentResponse
    -- ** Response lenses
    , gdrrsCustomMetadata
    , gdrrsMetadata
    , gdrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkGetDocument' smart constructor.
data GetDocument = GetDocument'
  { documentId :: Types.DocumentId
    -- ^ The ID of the document.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , includeCustomMetadata :: Core.Maybe Core.Bool
    -- ^ Set this to @TRUE@ to include custom metadata in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocument' value with any optional fields omitted.
mkGetDocument
    :: Types.DocumentId -- ^ 'documentId'
    -> GetDocument
mkGetDocument documentId
  = GetDocument'{documentId, authenticationToken = Core.Nothing,
                 includeCustomMetadata = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentId :: Lens.Lens' GetDocument Types.DocumentId
gdDocumentId = Lens.field @"documentId"
{-# INLINEABLE gdDocumentId #-}
{-# DEPRECATED documentId "Use generic-lens or generic-optics with 'documentId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAuthenticationToken :: Lens.Lens' GetDocument (Core.Maybe Types.AuthenticationHeaderType)
gdAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE gdAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | Set this to @TRUE@ to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdIncludeCustomMetadata :: Lens.Lens' GetDocument (Core.Maybe Core.Bool)
gdIncludeCustomMetadata = Lens.field @"includeCustomMetadata"
{-# INLINEABLE gdIncludeCustomMetadata #-}
{-# DEPRECATED includeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead"  #-}

instance Core.ToQuery GetDocument where
        toQuery GetDocument{..}
          = Core.maybe Core.mempty (Core.toQueryPair "includeCustomMetadata")
              includeCustomMetadata

instance Core.ToHeaders GetDocument where
        toHeaders GetDocument{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest GetDocument where
        type Rs GetDocument = GetDocumentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/api/v1/documents/" Core.<> Core.toText documentId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDocumentResponse' Core.<$>
                   (x Core..:? "CustomMetadata") Core.<*> x Core..:? "Metadata"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { customMetadata :: Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType)
    -- ^ The custom metadata on the document.
  , metadata :: Core.Maybe Types.DocumentMetadata
    -- ^ The metadata details of the document.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDocumentResponse' value with any optional fields omitted.
mkGetDocumentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDocumentResponse
mkGetDocumentResponse responseStatus
  = GetDocumentResponse'{customMetadata = Core.Nothing,
                         metadata = Core.Nothing, responseStatus}

-- | The custom metadata on the document.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsCustomMetadata :: Lens.Lens' GetDocumentResponse (Core.Maybe (Core.HashMap Types.CustomMetadataKeyType Types.CustomMetadataValueType))
gdrrsCustomMetadata = Lens.field @"customMetadata"
{-# INLINEABLE gdrrsCustomMetadata #-}
{-# DEPRECATED customMetadata "Use generic-lens or generic-optics with 'customMetadata' instead"  #-}

-- | The metadata details of the document.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsMetadata :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentMetadata)
gdrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE gdrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDocumentResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
