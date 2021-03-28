{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateDocumentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the document version to ACTIVE. 
--
-- Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by 'InitiateDocumentVersionUpload' . 
module Network.AWS.WorkDocs.UpdateDocumentVersion
    (
    -- * Creating a request
      UpdateDocumentVersion (..)
    , mkUpdateDocumentVersion
    -- ** Request lenses
    , udvDocumentId
    , udvVersionId
    , udvAuthenticationToken
    , udvVersionStatus

    -- * Destructuring the response
    , UpdateDocumentVersionResponse (..)
    , mkUpdateDocumentVersionResponse
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkUpdateDocumentVersion' smart constructor.
data UpdateDocumentVersion = UpdateDocumentVersion'
  { documentId :: Types.DocumentId
    -- ^ The ID of the document.
  , versionId :: Types.VersionId
    -- ^ The version ID of the document.
  , authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
    -- ^ Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
  , versionStatus :: Core.Maybe Types.DocumentVersionStatus
    -- ^ The status of the version.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentVersion' value with any optional fields omitted.
mkUpdateDocumentVersion
    :: Types.DocumentId -- ^ 'documentId'
    -> Types.VersionId -- ^ 'versionId'
    -> UpdateDocumentVersion
mkUpdateDocumentVersion documentId versionId
  = UpdateDocumentVersion'{documentId, versionId,
                           authenticationToken = Core.Nothing, versionStatus = Core.Nothing}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvDocumentId :: Lens.Lens' UpdateDocumentVersion Types.DocumentId
udvDocumentId = Lens.field @"documentId"
{-# INLINEABLE udvDocumentId #-}
{-# DEPRECATED documentId "Use generic-lens or generic-optics with 'documentId' instead"  #-}

-- | The version ID of the document.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvVersionId :: Lens.Lens' UpdateDocumentVersion Types.VersionId
udvVersionId = Lens.field @"versionId"
{-# INLINEABLE udvVersionId #-}
{-# DEPRECATED versionId "Use generic-lens or generic-optics with 'versionId' instead"  #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvAuthenticationToken :: Lens.Lens' UpdateDocumentVersion (Core.Maybe Types.AuthenticationHeaderType)
udvAuthenticationToken = Lens.field @"authenticationToken"
{-# INLINEABLE udvAuthenticationToken #-}
{-# DEPRECATED authenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead"  #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'versionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvVersionStatus :: Lens.Lens' UpdateDocumentVersion (Core.Maybe Types.DocumentVersionStatus)
udvVersionStatus = Lens.field @"versionStatus"
{-# INLINEABLE udvVersionStatus #-}
{-# DEPRECATED versionStatus "Use generic-lens or generic-optics with 'versionStatus' instead"  #-}

instance Core.ToQuery UpdateDocumentVersion where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateDocumentVersion where
        toHeaders UpdateDocumentVersion{..}
          = Core.toHeaders "Authentication" authenticationToken Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateDocumentVersion where
        toJSON UpdateDocumentVersion{..}
          = Core.object
              (Core.catMaybes [("VersionStatus" Core..=) Core.<$> versionStatus])

instance Core.AWSRequest UpdateDocumentVersion where
        type Rs UpdateDocumentVersion = UpdateDocumentVersionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PATCH,
                         Core._rqPath =
                           "/api/v1/documents/" Core.<> Core.toText documentId Core.<>
                             "/versions/"
                             Core.<> Core.toText versionId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull UpdateDocumentVersionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateDocumentVersionResponse' smart constructor.
data UpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentVersionResponse' value with any optional fields omitted.
mkUpdateDocumentVersionResponse
    :: UpdateDocumentVersionResponse
mkUpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
