{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    UpdateDocumentVersion (..),
    mkUpdateDocumentVersion,

    -- ** Request lenses
    udvDocumentId,
    udvVersionId,
    udvAuthenticationToken,
    udvVersionStatus,

    -- * Destructuring the response
    UpdateDocumentVersionResponse (..),
    mkUpdateDocumentVersionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkDocs.Types as Types

-- | /See:/ 'mkUpdateDocumentVersion' smart constructor.
data UpdateDocumentVersion = UpdateDocumentVersion'
  { -- | The ID of the document.
    documentId :: Types.DocumentId,
    -- | The version ID of the document.
    versionId :: Types.VersionId,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Core.Maybe Types.AuthenticationHeaderType,
    -- | The status of the version.
    versionStatus :: Core.Maybe Types.DocumentVersionStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentVersion' value with any optional fields omitted.
mkUpdateDocumentVersion ::
  -- | 'documentId'
  Types.DocumentId ->
  -- | 'versionId'
  Types.VersionId ->
  UpdateDocumentVersion
mkUpdateDocumentVersion documentId versionId =
  UpdateDocumentVersion'
    { documentId,
      versionId,
      authenticationToken = Core.Nothing,
      versionStatus = Core.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvDocumentId :: Lens.Lens' UpdateDocumentVersion Types.DocumentId
udvDocumentId = Lens.field @"documentId"
{-# DEPRECATED udvDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | The version ID of the document.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvVersionId :: Lens.Lens' UpdateDocumentVersion Types.VersionId
udvVersionId = Lens.field @"versionId"
{-# DEPRECATED udvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvAuthenticationToken :: Lens.Lens' UpdateDocumentVersion (Core.Maybe Types.AuthenticationHeaderType)
udvAuthenticationToken = Lens.field @"authenticationToken"
{-# DEPRECATED udvAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'versionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvVersionStatus :: Lens.Lens' UpdateDocumentVersion (Core.Maybe Types.DocumentVersionStatus)
udvVersionStatus = Lens.field @"versionStatus"
{-# DEPRECATED udvVersionStatus "Use generic-lens or generic-optics with 'versionStatus' instead." #-}

instance Core.FromJSON UpdateDocumentVersion where
  toJSON UpdateDocumentVersion {..} =
    Core.object
      (Core.catMaybes [("VersionStatus" Core..=) Core.<$> versionStatus])

instance Core.AWSRequest UpdateDocumentVersion where
  type Rs UpdateDocumentVersion = UpdateDocumentVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
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
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull UpdateDocumentVersionResponse'

-- | /See:/ 'mkUpdateDocumentVersionResponse' smart constructor.
data UpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocumentVersionResponse' value with any optional fields omitted.
mkUpdateDocumentVersionResponse ::
  UpdateDocumentVersionResponse
mkUpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
