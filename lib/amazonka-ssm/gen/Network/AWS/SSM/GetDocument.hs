{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Systems Manager document.
module Network.AWS.SSM.GetDocument
  ( -- * Creating a request
    GetDocument (..),
    mkGetDocument,

    -- ** Request lenses
    gdName,
    gdDocumentFormat,
    gdDocumentVersion,
    gdVersionName,

    -- * Destructuring the response
    GetDocumentResponse (..),
    mkGetDocumentResponse,

    -- ** Response lenses
    gdrrsAttachmentsContent,
    gdrrsContent,
    gdrrsDocumentFormat,
    gdrrsDocumentType,
    gdrrsDocumentVersion,
    gdrrsName,
    gdrrsRequires,
    gdrrsStatus,
    gdrrsStatusInformation,
    gdrrsVersionName,
    gdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | The name of the Systems Manager document.
    name :: Types.DocumentARN,
    -- | Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
    documentFormat :: Core.Maybe Types.DocumentFormat,
    -- | The document version for which you want information.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
    versionName :: Core.Maybe Types.DocumentVersionName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocument' value with any optional fields omitted.
mkGetDocument ::
  -- | 'name'
  Types.DocumentARN ->
  GetDocument
mkGetDocument name =
  GetDocument'
    { name,
      documentFormat = Core.Nothing,
      documentVersion = Core.Nothing,
      versionName = Core.Nothing
    }

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdName :: Lens.Lens' GetDocument Types.DocumentARN
gdName = Lens.field @"name"
{-# DEPRECATED gdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Returns the document in the specified format. The document format can be either JSON or YAML. JSON is the default format.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentFormat :: Lens.Lens' GetDocument (Core.Maybe Types.DocumentFormat)
gdDocumentFormat = Lens.field @"documentFormat"
{-# DEPRECATED gdDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The document version for which you want information.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentVersion :: Lens.Lens' GetDocument (Core.Maybe Types.DocumentVersion)
gdDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED gdDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | An optional field specifying the version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document and can't be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdVersionName :: Lens.Lens' GetDocument (Core.Maybe Types.DocumentVersionName)
gdVersionName = Lens.field @"versionName"
{-# DEPRECATED gdVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

instance Core.FromJSON GetDocument where
  toJSON GetDocument {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("DocumentFormat" Core..=) Core.<$> documentFormat,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            ("VersionName" Core..=) Core.<$> versionName
          ]
      )

instance Core.AWSRequest GetDocument where
  type Rs GetDocument = GetDocumentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetDocument")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Core.<$> (x Core..:? "AttachmentsContent")
            Core.<*> (x Core..:? "Content")
            Core.<*> (x Core..:? "DocumentFormat")
            Core.<*> (x Core..:? "DocumentType")
            Core.<*> (x Core..:? "DocumentVersion")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Requires")
            Core.<*> (x Core..:? "Status")
            Core.<*> (x Core..:? "StatusInformation")
            Core.<*> (x Core..:? "VersionName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | A description of the document attachments, including names, locations, sizes, and so on.
    attachmentsContent :: Core.Maybe [Types.AttachmentContent],
    -- | The contents of the Systems Manager document.
    content :: Core.Maybe Types.Content,
    -- | The document format, either JSON or YAML.
    documentFormat :: Core.Maybe Types.DocumentFormat,
    -- | The document type.
    documentType :: Core.Maybe Types.DocumentType,
    -- | The document version.
    documentVersion :: Core.Maybe Types.DocumentVersion,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Types.DocumentARN,
    -- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
    requires :: Core.Maybe (Core.NonEmpty Types.DocumentRequires),
    -- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
    status :: Core.Maybe Types.DocumentStatus,
    -- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
    statusInformation :: Core.Maybe Types.StatusInformation,
    -- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
    versionName :: Core.Maybe Types.DocumentVersionName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDocumentResponse' value with any optional fields omitted.
mkGetDocumentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetDocumentResponse
mkGetDocumentResponse responseStatus =
  GetDocumentResponse'
    { attachmentsContent = Core.Nothing,
      content = Core.Nothing,
      documentFormat = Core.Nothing,
      documentType = Core.Nothing,
      documentVersion = Core.Nothing,
      name = Core.Nothing,
      requires = Core.Nothing,
      status = Core.Nothing,
      statusInformation = Core.Nothing,
      versionName = Core.Nothing,
      responseStatus
    }

-- | A description of the document attachments, including names, locations, sizes, and so on.
--
-- /Note:/ Consider using 'attachmentsContent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsAttachmentsContent :: Lens.Lens' GetDocumentResponse (Core.Maybe [Types.AttachmentContent])
gdrrsAttachmentsContent = Lens.field @"attachmentsContent"
{-# DEPRECATED gdrrsAttachmentsContent "Use generic-lens or generic-optics with 'attachmentsContent' instead." #-}

-- | The contents of the Systems Manager document.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsContent :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.Content)
gdrrsContent = Lens.field @"content"
{-# DEPRECATED gdrrsContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The document format, either JSON or YAML.
--
-- /Note:/ Consider using 'documentFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDocumentFormat :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentFormat)
gdrrsDocumentFormat = Lens.field @"documentFormat"
{-# DEPRECATED gdrrsDocumentFormat "Use generic-lens or generic-optics with 'documentFormat' instead." #-}

-- | The document type.
--
-- /Note:/ Consider using 'documentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDocumentType :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentType)
gdrrsDocumentType = Lens.field @"documentType"
{-# DEPRECATED gdrrsDocumentType "Use generic-lens or generic-optics with 'documentType' instead." #-}

-- | The document version.
--
-- /Note:/ Consider using 'documentVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsDocumentVersion :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentVersion)
gdrrsDocumentVersion = Lens.field @"documentVersion"
{-# DEPRECATED gdrrsDocumentVersion "Use generic-lens or generic-optics with 'documentVersion' instead." #-}

-- | The name of the Systems Manager document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsName :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentARN)
gdrrsName = Lens.field @"name"
{-# DEPRECATED gdrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A list of SSM documents required by a document. For example, an @ApplicationConfiguration@ document requires an @ApplicationConfigurationSchema@ document.
--
-- /Note:/ Consider using 'requires' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsRequires :: Lens.Lens' GetDocumentResponse (Core.Maybe (Core.NonEmpty Types.DocumentRequires))
gdrrsRequires = Lens.field @"requires"
{-# DEPRECATED gdrrsRequires "Use generic-lens or generic-optics with 'requires' instead." #-}

-- | The status of the Systems Manager document, such as @Creating@ , @Active@ , @Updating@ , @Failed@ , and @Deleting@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsStatus :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentStatus)
gdrrsStatus = Lens.field @"status"
{-# DEPRECATED gdrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A message returned by AWS Systems Manager that explains the @Status@ value. For example, a @Failed@ status might be explained by the @StatusInformation@ message, "The specified S3 bucket does not exist. Verify that the URL of the S3 bucket is correct."
--
-- /Note:/ Consider using 'statusInformation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsStatusInformation :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.StatusInformation)
gdrrsStatusInformation = Lens.field @"statusInformation"
{-# DEPRECATED gdrrsStatusInformation "Use generic-lens or generic-optics with 'statusInformation' instead." #-}

-- | The version of the artifact associated with the document. For example, "Release 12, Update 6". This value is unique across all versions of a document, and cannot be changed.
--
-- /Note:/ Consider using 'versionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsVersionName :: Lens.Lens' GetDocumentResponse (Core.Maybe Types.DocumentVersionName)
gdrrsVersionName = Lens.field @"versionName"
{-# DEPRECATED gdrrsVersionName "Use generic-lens or generic-optics with 'versionName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrrsResponseStatus :: Lens.Lens' GetDocumentResponse Core.Int
gdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
