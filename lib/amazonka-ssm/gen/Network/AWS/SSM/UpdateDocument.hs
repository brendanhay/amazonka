{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.UpdateDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more values for an SSM document.
module Network.AWS.SSM.UpdateDocument
  ( -- * Creating a request
    UpdateDocument (..),
    mkUpdateDocument,

    -- ** Request lenses
    udContent,
    udName,
    udAttachments,
    udDocumentFormat,
    udDocumentVersion,
    udTargetType,
    udVersionName,

    -- * Destructuring the response
    UpdateDocumentResponse (..),
    mkUpdateDocumentResponse,

    -- ** Response lenses
    udrrsDocumentDescription,
    udrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | A valid JSON or YAML string.
    content :: Types.Content,
    -- | The name of the document that you want to update.
    name :: Types.DocumentName,
    -- | A list of key and value pairs that describe attachments to a version of a document.
    attachments :: Core.Maybe [Types.AttachmentsSource],
    -- | Specify the document format for the new document version. Systems Manager supports JSON and YAML documents. JSON is the default format.
    documentFormat :: Core.Maybe Types.DocumentFormat,
    -- | (Required) The latest version of the document that you want to update. The latest document version can be specified using the $LATEST variable or by the version number. Updating a previous version of a document is not supported.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | Specify a new target type for the document.
    targetType :: Core.Maybe Types.TargetType,
    -- | An optional field specifying the version of the artifact you are updating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
    versionName :: Core.Maybe Types.DocumentVersionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDocument' value with any optional fields omitted.
mkUpdateDocument ::
  -- | 'content'
  Types.Content ->
  -- | 'name'
  Types.DocumentName ->
  UpdateDocument
mkUpdateDocument content name =
  UpdateDocument'
    { content,
      name,
      attachments = Core.Nothing,
      documentFormat = Core.Nothing,
      documentVersion = Core.Nothing,
      targetType = Core.Nothing,
      versionName = Core.Nothing
    }

-- | A valid JSON or YAML string.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udContent :: Lens.Lens' UpdateDocument Types.Content
udContent = Lens.field @"content"
{-# DEPRECATED udContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The name of the document that you want to update.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDocument Types.DocumentName
udName = Lens.field @"name"
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of key and value pairs that describe attachments to a version of a document.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udAttachments :: Lens.Lens' UpdateDocument (Core.Maybe [Types.AttachmentsSource])
udAttachments = Lens.field @"attachments"
{-# DEPRECATED udAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | Specify the document format for the new document version. Systems Manager supports JSON and YAML documents. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocumentFormat :: Lens.Lens' UpdateDocument (Core.Maybe Types.DocumentFormat)
udDocumentFormat = Lens.field @"documentFormat"
{-# DEPRECATED udDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | (Required) The latest version of the document that you want to update. The latest document version can be specified using the $LATEST variable or by the version number. Updating a previous version of a document is not supported.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocumentVersion :: Lens.Lens' UpdateDocument (Core.Maybe Types.DocumentVersion)
udDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED udDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | Specify a new target type for the document.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udTargetType :: Lens.Lens' UpdateDocument (Core.Maybe Types.TargetType)
udTargetType = Lens.field @"targetType"
{-# DEPRECATED udTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

-- | An optional field specifying the version of the artifact you are updating with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udVersionName :: Lens.Lens' UpdateDocument (Core.Maybe Types.DocumentVersionName)
udVersionName = Lens.field @"versionName"
{-# DEPRECATED udVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

instance Core.FromJSON UpdateDocument where
  toJSON UpdateDocument {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Content" Core..= content),
            Core.Just ("Name" Core..= name),
            ("Attachments" Core..=) Core.<$> attachments,
            ("DocumentFormat" Core..=) Core.<$> documentFormat,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("TargetType" Core..=) Core.<$> targetType,
            ("VersionName" Core..=) Core.<$> versionName
          ]
      )

instance Core.AWSRequest UpdateDocument where
  type Rs UpdateDocument = UpdateDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.UpdateDocument")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDocumentResponse'
            Core.<$> (x Core..:? "DocumentDescription")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  { -- | A description of the document that was updated.
    documentDescription :: Core.Maybe Types.DocumentDescription,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateDocumentResponse' value with any optional fields omitted.
mkUpdateDocumentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDocumentResponse
mkUpdateDocumentResponse responseStatus =
  UpdateDocumentResponse'
    { documentDescription = Core.Nothing,
      responseStatus
    }

-- | A description of the document that was updated.
--
-- /Note:/ Consider using 'documentDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsDocumentDescription :: Lens.Lens' UpdateDocumentResponse (Core.Maybe Types.DocumentDescription)
udrrsDocumentDescription = Lens.field @"documentDescription"
{-# DEPRECATED udrrsDocumentDescription "Use generic-lens or generic-optics with 'documentDescription' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udrrsResponseStatus :: Lens.Lens' UpdateDocumentResponse Core.Int
udrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
