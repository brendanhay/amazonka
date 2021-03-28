{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.AbortDocumentVersionUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Aborts the upload of the specified document version that was previously initiated by 'InitiateDocumentVersionUpload' . The client should make this call only when it no longer intends to upload the document version, or fails to do so.
module Network.AWS.WorkDocs.AbortDocumentVersionUpload
    (
    -- * Creating a request
      AbortDocumentVersionUpload (..)
    , mkAbortDocumentVersionUpload
    -- ** Request lenses
    , advuDocumentId
    , advuVersionId
    , advuAuthenticationToken

    -- * Destructuring the response
    , AbortDocumentVersionUploadResponse (..)
    , mkAbortDocumentVersionUploadResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkAbortDocumentVersionUpload' smart constructor.
data AbortDocumentVersionUpload = AbortDocumentVersionUpload'
  { documentId :: Types.ResourceIdType
    -- ^ The ID of the document.
  , versionId :: Types.VersionId
    -- ^ The ID of the version.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortDocumentVersionUpload' value with any optional fields omitted.
mkAbortDocumentVersionUpload
    :: Types.ResourceIdType -- ^ 'documentId'
    -> Types.VersionId -- ^ 'versionId'
    -> AbortDocumentVersionUpload
mkAbortDocumentVersionUpload documentId versionId
  = AbortDocumentVersionUpload'{documentId, versionId,
                                authenticationToken = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuDocumentId :: Lens.Lens' AbortDocumentVersionUpload Types.ResourceIdType
advuDocumentId = Lens.field @"documentId"
{-# INLINEABLE advuDocumentId #-}
{-# DEPRECATED documentId "Use generic-lens or generic-optics with 'documentId' instead"  #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuVersionId :: Lens.Lens' AbortDocumentVersionUpload Types.VersionId
advuVersionId = Lens.field @"versionId"
{-# INLINEABLE advuVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuAuthenticationToken :: Lens.Lens' AbortDocumentVersionUpload (Core.Maybe Types.AuthenticationHeaderType)
advuAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE advuAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

instance Core.ToQuery AbortDocumentVersionUpload where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders AbortDocumentVersionUpload where
        toHeaders AbortDocumentVersionUpload{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest AbortDocumentVersionUpload where
        type Rs AbortDocumentVersionUpload =
             AbortDocumentVersionUploadResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/api/v1/documents/" Core.<> Core.toText documentId Core.<>
                             "/versions/"
                             Core.<> Core.toText versionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveNull AbortDocumentVersionUploadResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkAbortDocumentVersionUploadResponse' smart constructor.
data AbortDocumentVersionUploadResponse = AbortDocumentVersionUploadResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortDocumentVersionUploadResponse' value with any optional fields omitted.
mkAbortDocumentVersionUploadResponse
    :: AbortDocumentVersionUploadResponse
mkAbortDocumentVersionUploadResponse
  = AbortDocumentVersionUploadResponse'
