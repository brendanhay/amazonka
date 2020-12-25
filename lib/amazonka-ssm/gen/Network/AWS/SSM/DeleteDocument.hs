{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DeleteDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the Systems Manager document and all instance associations to the document.
--
-- Before you delete the document, we recommend that you use 'DeleteAssociation' to disassociate all instances that are associated with the document.
module Network.AWS.SSM.DeleteDocument
  ( -- * Creating a request
    DeleteDocument (..),
    mkDeleteDocument,

    -- ** Request lenses
    ddfName,
    ddfDocumentVersion,
    ddfForce,
    ddfVersionName,

    -- * Destructuring the response
    DeleteDocumentResponse (..),
    mkDeleteDocumentResponse,

    -- ** Response lenses
    ddrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | The name of the document.
    name :: Types.DocumentName,
    -- | The version of the document that you want to delete. If not provided, all versions of the document are deleted.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
    force :: Core.Maybe Core.Bool,
    -- | The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
    versionName :: Core.Maybe Types.DocumentVersionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocument' value with any optional fields omitted.
mkDeleteDocument ::
  -- | 'name'
  Types.DocumentName ->
  DeleteDocument
mkDeleteDocument name =
  DeleteDocument'
    { name,
      documentVersion = Core.Nothing,
      force = Core.Nothing,
      versionName = Core.Nothing
    }

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfName :: Lens.Lens' DeleteDocument Types.DocumentName
ddfName = Lens.field @"name"
{-# DEPRECATED ddfName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDocumentVersion :: Lens.Lens' DeleteDocument (Core.Maybe Types.DocumentVersion)
ddfDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED ddfDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Some SSM document types require that you specify a @Force@ flag before you can delete the document. For example, you must specify a @Force@ flag to delete a document of type @ApplicationConfigurationSchema@ . You can restrict access to the @Force@ flag in an AWS Identity and Access Management (IAM) policy.
--
-- /Note:/ Consider using 'force' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfForce :: Lens.Lens' DeleteDocument (Core.Maybe Core.Bool)
ddfForce = Lens.field @"force"
{-# DEPRECATED ddfForce "Use generic-lens or generic-optics with 'force' instead." #-}

-- | The version name of the document that you want to delete. If not provided, all versions of the document are deleted.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfVersionName :: Lens.Lens' DeleteDocument (Core.Maybe Types.DocumentVersionName)
ddfVersionName = Lens.field @"versionName"
{-# DEPRECATED ddfVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

instance Core.FromJSON DeleteDocument where
  toJSON DeleteDocument {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("Force" Core..=) Core.<$> force,
            ("VersionName" Core..=) Core.<$> versionName
          ]
      )

instance Core.AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.DeleteDocument")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteDocumentResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteDocumentResponse' smart constructor.
newtype DeleteDocumentResponse = DeleteDocumentResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDocumentResponse' value with any optional fields omitted.
mkDeleteDocumentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteDocumentResponse
mkDeleteDocumentResponse responseStatus =
  DeleteDocumentResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrfrsResponseStatus :: Lens.Lens' DeleteDocumentResponse Core.Int
ddrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
