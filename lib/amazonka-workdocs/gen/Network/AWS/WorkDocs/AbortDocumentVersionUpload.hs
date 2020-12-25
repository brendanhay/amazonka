{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    AbortDocumentVersionUpload (..),
    mkAbortDocumentVersionUpload,

    -- ** Request lenses
    advuDocumentId,
    advuVersionId,
    advuAuthenticationToken,

    -- * Destructuring the response
    AbortDocumentVersionUploadResponse (..),
    mkAbortDocumentVersionUploadResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkAbortDocumentVersionUpload' smart constructor.
data AbortDocumentVersionUpload = AbortDocumentVersionUpload'
  { -- | The ID of the document.
    documentId :: Types.ResourceIdType,
    -- | The ID of the version.
    versionId :: Types.VersionId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortDocumentVersionUpload' value with any optional fields omitted.
mkAbortDocumentVersionUpload ::
  -- | 'documentId'
  Types.ResourceIdType ->
  -- | 'versionId'
  Types.VersionId ->
  AbortDocumentVersionUpload
mkAbortDocumentVersionUpload documentId versionId =
  AbortDocumentVersionUpload'
    { documentId,
      versionId,
      authenticationToken = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuDocumentId :: Lens.Lens' AbortDocumentVersionUpload Types.ResourceIdType
advuDocumentId = Lens.field @"documentId"
{-# DEPRECATED advuDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | The ID of the version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuVersionId :: Lens.Lens' AbortDocumentVersionUpload Types.VersionId
advuVersionId = Lens.field @"versionId"
{-# DEPRECATED advuVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuAuthenticationToken :: Lens.Lens' AbortDocumentVersionUpload (Core.Maybe Types.AuthenticationHeaderType)
advuAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED advuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Core.AWSRequest AbortDocumentVersionUpload where
  type
    Rs AbortDocumentVersionUpload =
      AbortDocumentVersionUploadResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.DELETE,
        Core._rqPath =
          Core.rawPath
            ( "/api/v1/documents/" Core.<> (Core.toText documentId)
                Core.<> ("/versions/")
                Core.<> (Core.toText versionId)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.toHeaders "Authentication" authenticationToken
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = ""
      }
  response = Response.receiveNull AbortDocumentVersionUploadResponse'

-- | /See:/ 'mkAbortDocumentVersionUploadResponse' smart constructor.
data AbortDocumentVersionUploadResponse = AbortDocumentVersionUploadResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AbortDocumentVersionUploadResponse' value with any optional fields omitted.
mkAbortDocumentVersionUploadResponse ::
  AbortDocumentVersionUploadResponse
mkAbortDocumentVersionUploadResponse =
  AbortDocumentVersionUploadResponse'
